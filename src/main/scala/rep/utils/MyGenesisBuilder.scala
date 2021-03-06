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

import java.io.PrintWriter

/**
 * 用于生成创世块json文件,该json文件可以在链初始化时由节点加载
 * 创世块中预置了deploy基础方法的交易
 *@author shidianyue
 */
object MyGenesisBuilder {
  val jks_path = "jks/"
  val cer_path = "jks/certs/"
  implicit val serialization = jackson.Serialization // or native.Serialization
  implicit val formats       = DefaultFormats

  def main(args: Array[String]): Unit = {
    SignTool.loadPrivateKey("121000005l35120456.node1", "123", jks_path + "121000005l35120456.node1.jks")
    SignTool.loadNodeCertList("changeme", jks_path + "mytruststore.jks")
    SignTool.loadPrivateKey("951002007l78123233.super_admin", "super_admin",  jks_path + "951002007l78123233.super_admin.jks")
    val sysName = "121000005l35120456.node1"
    //交易发起人是超级管理员
    //增加scala的资产管理合约   
    // read deploy funcs
    val s1 = scala.io.Source.fromFile("src/main/scala/rep/sc/tpl/ContractCert.scala","UTF-8")
    val l1 = try s1.mkString finally s1.close()
    val cid = new ChaincodeId("ContractCert",1)
    val dep_trans = PeerHelper.createTransaction4Deploy("951002007l78123233.super_admin", cid,
      l1, "",5000, rep.protos.peer.ChaincodeDeploy.CodeType.CODE_SCALA)

    // 交易审查合约
    val tr = scala.io.Source.fromFile("src/main/scala/rep/sc/tpl/TransactionReview.scala","UTF-8")
    val trL = try tr.mkString finally tr.close()
    val trCid = new ChaincodeId("TransactionReview",1)
    val tr_dep_trans = PeerHelper.createTransaction4Deploy("951002007l78123233.super_admin", trCid,
      trL, "",5000, rep.protos.peer.ChaincodeDeploy.CodeType.CODE_SCALA)

    var translist : Array[Transaction] = new Array[Transaction] (28)

    //val dep_trans = PeerHelper.createTransaction4Deploy(sysName, cid,
    //           l1, "",5000, rep.protos.peer.ChaincodeDeploy.CodeType.CODE_SCALA)

    translist(0) = dep_trans
    translist(1) = tr_dep_trans
    
    //val dep_trans_state = PeerHelper.createTransaction4State(sysName, cid, true)
    //translist(1) = dep_trans_state
    
    //System.out.println(Json4s.compactJson(dep_trans))
    
    var signers : Array[Signer] = new Array[Signer](12)
    signers(0) = Signer("node1","121000005l35120456","18912345678",List("node1")) 
    signers(1) = Signer("node2","12110107bi45jh675g","18912345678",List("node2")) 
    signers(2) = Signer("node3","122000002n00123567","18912345678",List("node3")) 
    signers(3) = Signer("node4","921000005k36123789","18912345678",List("node4")) 
    signers(4) = Signer("node5","921000006e0012v696","18912345678",List("node5")) 
    signers(5) = Signer("super_admin","951002007l78123233","18912345678",List("super_admin"))


    signers(6) = Signer("deployer1","1acd7866562e47c7bc","18912345678",List("deployer1"))

    signers(7) = Signer("scorer1","766beb6e69304d76b0","18912345678",List("scorer1"))
    signers(8) = Signer("scorer2","3bed7098ba174a7093","18912345678",List("scorer2"))

    signers(9) = Signer("supervisor1","bc33470bf3714934b6","18912345678",List("supervisor1"))
    signers(10) = Signer("supervisor2","8a225e08262a4d24b2","18912345678",List("supervisor2"))
    signers(11) = Signer("supervisor3","0ace920324094eec8a","18912345678",List("supervisor3"))


    
    
    for(i<-0 to 11){
        translist(i+2) = PeerHelper.createTransaction4Invoke("951002007l78123233.super_admin", cid,
                    "SignUpSigner", Seq(SerializeUtils.compactJson(signers(i))))
    }
    
    //注册节点
    for(i<-0 to 5){
      val certfile = scala.io.Source.fromFile(cer_path + signers(i).creditCode+"."+signers(i).name+".cer","UTF-8")
      val certstr = try certfile.mkString finally certfile.close()
     // val cert = SignTool.getCertByFile("jks/"+signers(i).creditCode+"."+signers(i).name+".cer")
      val millis = System.currentTimeMillis()
      
      val tmp = rep.protos.peer.Certificate(certstr,"SHA1withECDSA",true,Option(Timestamp(millis/1000 , ((millis % 1000) * 1000000).toInt)))
       //val aa = new ContractCert
      val a : CertInfo = CertInfo(signers(i).creditCode,signers(i).name,tmp)
      translist(i+14) = PeerHelper.createTransaction4Invoke("951002007l78123233.super_admin", cid,
                    "SignUpCert", Seq(SerializeUtils.compactJson(a)))
    }

    //注册合约部署
    for(i<-6 to 6){
      val certfile = scala.io.Source.fromFile("jks_200/deployers/"+signers(i).creditCode+"."+signers(i).name+".cer","UTF-8")
      val certstr = try certfile.mkString finally certfile.close()
      // val cert = SignTool.getCertByFile("jks/"+signers(i).creditCode+"."+signers(i).name+".cer")
      val millis = System.currentTimeMillis()

      val tmp = rep.protos.peer.Certificate(certstr,"SHA1withECDSA",true,Option(Timestamp(millis/1000 , ((millis % 1000) * 1000000).toInt)))
      //val aa = new ContractCert
      val a : CertInfo = CertInfo(signers(i).creditCode,signers(i).name,tmp)
      translist(i + 14) = PeerHelper.createTransaction4Invoke("951002007l78123233.super_admin", cid,
        "SignUpCert", Seq(SerializeUtils.compactJson(a)))
    }

    //注册积分管理
    for(i<-7 to 8){
      val certfile = scala.io.Source.fromFile("jks_200/scorers/"+signers(i).creditCode+"."+signers(i).name+".cer","UTF-8")
      val certstr = try certfile.mkString finally certfile.close()
      // val cert = SignTool.getCertByFile("jks/"+signers(i).creditCode+"."+signers(i).name+".cer")
      val millis = System.currentTimeMillis()

      val tmp = rep.protos.peer.Certificate(certstr,"SHA1withECDSA",true,Option(Timestamp(millis/1000 , ((millis % 1000) * 1000000).toInt)))
      //val aa = new ContractCert
      val a : CertInfo = CertInfo(signers(i).creditCode,signers(i).name,tmp)
      translist(i + 14) = PeerHelper.createTransaction4Invoke("951002007l78123233.super_admin", cid,
        "SignUpCert", Seq(SerializeUtils.compactJson(a)))
    }

    // 注册监管员
    for(i<-9 to 11){
      val certfile = scala.io.Source.fromFile("jks_200/supervisors/"+signers(i).creditCode+"."+signers(i).name+".cer","UTF-8")
      val certstr = try certfile.mkString finally certfile.close()
      // val cert = SignTool.getCertByFile("jks/"+signers(i).creditCode+"."+signers(i).name+".cer")
      val millis = System.currentTimeMillis()

      val tmp = rep.protos.peer.Certificate(certstr,"SHA1withECDSA",true,Option(Timestamp(millis/1000 , ((millis % 1000) * 1000000).toInt)))
      //val aa = new ContractCert
      val a : CertInfo = CertInfo(signers(i).creditCode,signers(i).name,tmp)
      translist(i + 14) = PeerHelper.createTransaction4Invoke("951002007l78123233.super_admin", cid,
        "SignUpCert", Seq(SerializeUtils.compactJson(a)))
    }

     val s2 = scala.io.Source.fromFile("src/main/scala/rep/sc/tpl/ContractAssetsTPL.scala","UTF-8")
    val c2 = try s2.mkString finally s2.close()
    val cid2 = new ChaincodeId("ContractAssetsTPL",1)
    val dep_asserts_trans = PeerHelper.createTransaction4Deploy(sysName, cid2,
               c2, "",5000, rep.protos.peer.ChaincodeDeploy.CodeType.CODE_SCALA)
    translist(26) = dep_asserts_trans
    
    // read invoke scala contract
    val s3 = scala.io.Source.fromFile("api_req/json/set.json","UTF-8")
    val ct1 = try s3.mkString finally s3.close()
    
    translist(27) = PeerHelper.createTransaction4Invoke("951002007l78123233.super_admin", cid2,
                    "set", Seq(ct1))

    //create gensis block
    val millis = ConfigFactory.load().getLong("akka.genesisblock.creationBlockTime")
     
    var blk = new Block(1,1,translist,Seq(),_root_.com.google.protobuf.ByteString.EMPTY,
        _root_.com.google.protobuf.ByteString.EMPTY)
     
    //获得管理员证书和签名
//    val (priKA, pubKA, certA) = ECDSASign.getKeyPair("super_admin")
//    val (prik, pubK, cert) = ECDSASign.getKeyPair("1")
    //val blk_hash = blk.toByteArray
    //签名之前不再使用hash
    //val blk_hash = Sha256.hash(blk.toByteArray)
    //超级管理员背书（角色）
    //创建者背书（1）
    /*blk = blk.withEndorsements(Seq(
        BlockHelp.SignDataOfBlock(blk_hash,"951002007l78123233.super_admin"),
        BlockHelp.SignDataOfBlock(blk_hash,"121000005l35120456.node1")))*/
        blk = blk.clearEndorsements
        blk = blk.clearTransactionResults
    val r = JsonFormat.toJson(blk)   
    val rstr = pretty(render(r))
    println(rstr)

    val pw = new PrintWriter("json/gensis.json","UTF-8")
    pw.write(rstr)
    pw.flush()
    pw.close()
  }
}