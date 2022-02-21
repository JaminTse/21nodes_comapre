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

package rep.utils

import rep.crypto.cert.SignTool
import rep.network.PeerHelper

import java.io.{File, FilenameFilter, PrintWriter}

/**
 * 用于生成创世块json文件,该json文件可以在链初始化时由节点加载
 * 创世块中预置了deploy基础方法的交易
 *
 * @author shidianyue
 */
object MyGenesisBuilder2 {
  val jks_path = "jks/"
  val cer_path = "jks/certs/"
  val jks_pwd = "iscas@123"
  implicit val serialization: Serialization.type = jackson.Serialization // or native.Serialization
  implicit val formats: DefaultFormats.type = DefaultFormats

  def main(args: Array[String]): Unit = {
    SignTool.loadPrivateKey("121000005l35120456.node1", jks_pwd, jks_path + "121000005l35120456.node1.jks")
    SignTool.loadNodeCertList("changeme", jks_path + "mytruststore.jks")
    SignTool.loadPrivateKey("951002007l78123233.super_admin", "super_admin", jks_path + "951002007l78123233.super_admin.jks")
    val sysName = "121000005l35120456.node1"
    val transactionList = ListBuffer[Transaction]()

    val s1 = scala.io.Source.fromFile("src/main/scala/rep/sc/tpl/ContractCert.scala", "UTF-8")
    val l1 = try s1.mkString finally s1.close()
    val cid = new ChaincodeId("ContractCert", 1)
    val contractCert = PeerHelper.createTransaction4Deploy("951002007l78123233.super_admin", cid,
      l1, "", 5000, rep.protos.peer.ChaincodeDeploy.CodeType.CODE_SCALA)
    transactionList.+=(contractCert)

    val s2 = scala.io.Source.fromFile("src/main/scala/rep/sc/tpl/ContractAssetsTPL.scala", "UTF-8")
    val c2 = try s2.mkString finally s2.close()
    val cid2 = new ChaincodeId("ContractAssetsTPL", 1)
    val contractAssetsTPL = PeerHelper.createTransaction4Deploy(sysName, cid2,
      c2, "", 5000, rep.protos.peer.ChaincodeDeploy.CodeType.CODE_SCALA)
    transactionList.+=(contractAssetsTPL)

    // 交易审查合约
    val tr = scala.io.Source.fromFile("src/main/scala/rep/sc/tpl/TransactionReview.scala", "UTF-8")
    val trL = try tr.mkString finally tr.close()
    val trCid = new ChaincodeId("TransactionReview", 1)
    val transactionReview = PeerHelper.createTransaction4Deploy("951002007l78123233.super_admin", trCid,
      trL, "", 5000, rep.protos.peer.ChaincodeDeploy.CodeType.CODE_SCALA)

    transactionList.+=(transactionReview)

    /*======================  注册账户 =========================*/

    val file = new File(cer_path)
    val cerFileList = file.listFiles(new FilenameFilter {
      override def accept(dir: File, name: String): Boolean = {
        name.toLowerCase.endsWith(".cer")
      }
    })
    println(s"cerFileList:${cerFileList.length}")
    val signers: Array[Signer] = new Array[Signer](cerFileList.length)
    var i = 0
    for (cerFile <- cerFileList) {
      val arr = cerFile.getName.split("\\.")
      signers(i) = Signer(arr(1), arr(0), "123456789", List(arr(1)))
      println(signers(i))
      i = i + 1
    }

    for (i <- signers.indices) {
      val transaction = PeerHelper.createTransaction4Invoke("951002007l78123233.super_admin", cid,
        "SignUpSigner", Seq(SerializeUtils.compactJson(signers(i))))
      transactionList.+=(transaction)
    }
    println(s"transactions after SignUpSigner:${transactionList.size}")
    /*======================  注册证书 =========================*/
    for (i <- signers.indices) {
      val certfile = scala.io.Source.fromFile(cer_path + signers(i).creditCode + "." + signers(i).name + ".cer", "UTF-8")
      val certstr = try certfile.mkString finally certfile.close()
      val millis = System.currentTimeMillis()
      val tmp = rep.protos.peer.Certificate(certstr, "SHA1withECDSA", certValid = true, Option(Timestamp(millis / 1000, ((millis % 1000) * 1000000).toInt)))
      val a: CertInfo = CertInfo(signers(i).creditCode, signers(i).name, tmp)
      val transaction = PeerHelper.createTransaction4Invoke("951002007l78123233.super_admin", cid,
        "SignUpCert", Seq(SerializeUtils.compactJson(a)))
      transactionList.+=(transaction)
    }
    println(s"transactions after SignUpCert:${transactionList.size}")

    val s3 = scala.io.Source.fromFile("api_req/json/set.json", "UTF-8")
    val ct1 = try s3.mkString finally s3.close()
    val trans = PeerHelper.createTransaction4Invoke("951002007l78123233.super_admin", cid2,
      "set", Seq(ct1))
    transactionList.+=(trans)

    val tansactionSeq = transactionList.toSeq.sortWith((t1, t2) => {

      t1.getSignature.tmLocal.get.seconds < t2.getSignature.tmLocal.get.seconds
        //t1.getSignature.tmLocal.get.nanos < t2.getSignature.tmLocal.get.nanos
    })
    println(tansactionSeq.length)
    val millis = ConfigFactory.load().getLong("akka.genesisblock.creationBlockTime")
    var blk = new Block(1, 1, tansactionSeq, Seq(), _root_.com.google.protobuf.ByteString.EMPTY,
      _root_.com.google.protobuf.ByteString.EMPTY)

    blk = blk.clearEndorsements
    blk = blk.clearTransactionResults
    val r = JsonFormat.toJson(blk)
    val rstr = pretty(render(r))

    val pw = new PrintWriter("json/gensis.json", "UTF-8")
    pw.write(rstr)
    pw.flush()
    pw.close()
  }
}