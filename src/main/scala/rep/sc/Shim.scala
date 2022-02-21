/*
 * Copyright  2019 Blockchain Technology and Application Joint Lab, Linkel Technology Co., Ltd, Beijing, Fintech Research Center of ISCAS.
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BA SIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package rep.sc

import com.fasterxml.jackson.core.Base64Variants
import akka.actor.ActorSystem
import rep.network.tools.PeerExtension
import rep.protos.peer.{OperLog, Transaction}
import rep.storage.ImpDataPreload
import rep.utils.SerializeUtils
import rep.utils.SerializeUtils.deserialise
import rep.utils.SerializeUtils.serialise

import java.security.cert.CertificateFactory
import java.io.FileInputStream
import java.io.ByteArrayInputStream
import fastparse.utils.Base64

import java.io.StringReader
import java.security.cert.X509Certificate
import rep.storage.ImpDataAccess
import rep.crypto.cert.SignTool
import _root_.com.google.protobuf.ByteString
import rep.log.RepLogger
import org.slf4j.Logger

import java.util.concurrent.ConcurrentHashMap;

/** Shim伴生对象
 *  @author c4w
 *
 */
object Shim {

  type Key = String
  type Value = Array[Byte]

  import rep.storage.IdxPrefix._
  val ERR_CERT_EXIST = "证书已存在"
  val PRE_CERT_INFO = "CERT_INFO_"
  val PRE_CERT = "CERT_"
  val NOT_PERR_CERT = "非节点证书"
}

/** 为合约容器提供底层API的类
 *  @author c4w
 * @constructor 根据actor System和合约的链码id建立shim实例
 * @param system 所属的actorSystem
 * @param cid 合约的链码id
 */
class Shim(system: ActorSystem, cName: String) {

  import Shim._
  import rep.storage.IdxPrefix._

  val PRE_SPLIT = "_"
  //本chaincode的 key前缀
  val pre_key = WorldStateKeyPreFix + cName + PRE_SPLIT
  //存储模块提供的system单例
  val pe = PeerExtension(system)
  //从交易传入, 内存中的worldState快照
  var sr:ImpDataPreload = null
  //记录状态修改日志
  var ol = scala.collection.mutable.ListBuffer.empty[OperLog]

  /*********************** add by 李泽明 begin**********************/
  val point_pre_key = "points_"
  val record_pre_key = "record_"
  var index = 1
  private var record_list = new ConcurrentHashMap[String, ConcurrentHashMap[String,Array[Byte]]]

  // 积分充值
  def addPoints(key: Key, points: Int): Unit = {
    val pkey = point_pre_key + key
    // 获取当前积分余额
    val currentPoints = deserialise(get(point_pre_key + key)).asInstanceOf[Int]
    println("#### 当前账户： " + pkey + " ####")
    println("#### 当前积分余额为： " + currentPoints + " ####")

    // 更新积分
    val spoints = currentPoints + points
    this.sr.Put(pkey, serialise(spoints))
    println("#### 充值后积分余额： " + deserialise(get(point_pre_key + key)).asInstanceOf[Int] + " ####")
  }

  // 判断积分是否足够调用，若积分足够，则根据所需cost进行扣费，并记录本次消费
  def checkPoints(key: Key, cname: String, cost: Int): Boolean = {
    val pkey = point_pre_key + key
    // 获取当前积分余额
    var currentPoints = deserialise(get(pkey)).asInstanceOf[Int]
    println("#### 当前账户： " + pkey + " ####")
    println("#### 当前积分余额为： " + currentPoints + " ####")
    // 余额不足 返回false
    if(currentPoints < cost) {
      println("#### 积分余额不足，checkPoints返回false ####")
      false
    }
    else {  // 余额充足，进行扣费与积分的更新，并返回true
      currentPoints -= cost
      this.sr.Put(pkey, serialise(currentPoints))
      println("#### 积分余额足够 ####")
      println("#### 进行扣费，扣费后积分余额为： " + deserialise(get(point_pre_key + key)).asInstanceOf[Int] + " ####")

      // 对本次消费进行记录
      // Key: record_121000005l35120456_BusinessTestTPL   Value: 50
      var records = new ConcurrentHashMap[String, Array[Byte]]
      val rkey = record_pre_key + index + "_" + key + "_" + cname
      index += 1
      val spoints = serialise(currentPoints)
      if (!record_list.containsKey(key)) {
        records.put(rkey, spoints)
        record_list.put(key, records)
      } else if (record_list.containsKey(key) && record_list.get(key)!=null) {
        records = record_list.get(key)
        records.put(rkey, spoints)
      }
      // println("#### 本次消费记录： ####")
      // println("#### Key: " + rkey + "  Value: " + deserialise(record_list.get(key).get(rkey)) + " ####")
      // getRecord(key)
      true
    }
  }
  /*********************** add by 李泽明 end **********************/

  def setVal(key: Key, value: Any):Unit ={
    setState(key, serialise(value))
  }
  def getVal(key: Key):Any ={
    deserialise(getState(key))
  }

  def setState(key: Key, value: Array[Byte]): Unit = {
    val pkey = pre_key + key
    val oldValue = get(pkey)
    sr.Put(pkey, value)
    val ov = if(oldValue == null) ByteString.EMPTY else ByteString.copyFrom(oldValue)
    val nv = if(value == null) ByteString.EMPTY else ByteString.copyFrom(value)
    //记录操作日志
    //getLogger.trace(s"nodename=${sr.getSystemName},dbname=${sr.getInstanceName},txid=${txid},key=${key},old=${deserialise(oldValue)},new=${deserialise(value)}")
    ol += new OperLog(key,ov, nv)
  }

  private def get(key: Key): Array[Byte] = {
    sr.Get(key)
  }

  def getState(key: Key): Array[Byte] = {
    get(pre_key + key)
  }

  def getStateEx(cName:String, key: Key): Array[Byte] = {
    get(WorldStateKeyPreFix + cName + PRE_SPLIT + key)
  }

  //判断账号是否节点账号 TODO
  def bNodeCreditCode(credit_code: String) : Boolean ={
    SignTool.isNode4Credit(credit_code)
  }

  //通过该接口获取日志器，合约使用此日志器输出业务日志。
  def getLogger:Logger={
    RepLogger.Business_Logger
  }

  /*************************** 以下方法仅用于设置或查询非法交易id ****************************/
  def setPrimaryVal(k:String, v:Any):Unit = {
    val value = serialise(v)
    val oldValue = get(k)
    sr.Put(k, value)
    val ov = if(oldValue == null) ByteString.EMPTY else ByteString.copyFrom(oldValue)
    val nv = if(value == null) ByteString.EMPTY else ByteString.copyFrom(value)
    //记录操作日志
    //getLogger.trace(s"nodename=${sr.getSystemName},dbname=${sr.getInstanceName},txid=${txid},key=${key},old=${deserialise(oldValue)},new=${deserialise(value)}")
    ol += new OperLog(k,ov, nv)
  }

  def getPrimaryVal(key: Key):Any = {
    deserialise(get(key))
  }
}