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

import akka.actor.ActorSystem
import akka.testkit.{TestKit, TestProbe}
import org.json4s.{DefaultFormats, jackson}
import org.json4s.native.Serialization.{write, writePretty}
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike, Matchers}
import rep.app.system.ClusterSystem
import rep.app.system.ClusterSystem.InitType
import rep.network.PeerHelper
import rep.network.module.ModuleManager
import rep.protos.peer.{Certificate, ChaincodeId, Signer}
import rep.sc.SandboxSpec.{ACTION, SetMap}
import rep.sc.tpl._
//.{CertStatus,CertInfo}
import rep.sc.tpl.Transfer
import rep.storage.ImpDataAccess
import rep.utils.SerializeUtils.toJson
import rep.app.conf.SystemProfile

import scala.concurrent.duration._
import scala.collection.mutable.Map
import rep.sc.SandboxDispatcher.DoTransaction

object SandboxSpec {

  type SetMap = scala.collection.mutable.Map[String, Int]

  object ACTION {
    val transfer = "transfer"
    val set = "set"
    val SignUpSigner = "SignUpSigner"
    val SignUpCert = "SignUpCert"
    val UpdateCertStatus = "UpdateCertStatus"
    val UpdateSigner = "UpdateSigner"
  }

}

/**
  * author zyf
  *
  * @param _system
  */
class SandboxSpec(_system: ActorSystem)
  extends TestKit(_system) with Matchers with FlatSpecLike with BeforeAndAfterAll {

  def this() = this(ActorSystem("TransferSpec", new ClusterSystem("121000005l35120456.node1", InitType.MULTI_INIT, false).getConf))

  override def afterAll: Unit = {
    shutdown(system)
  }

  implicit val serialization = jackson.Serialization
  // or native.Serialization
  implicit val formats = DefaultFormats

  "ContractAssetsTPL" should "can set assets and transfer from a to b" in {
    val sysName = "121000005l35120456.node1"
    val dbTag = "121000005l35120456.node1"
    //??????PeerManager?????????????????????transactionCreator(????????????????????????)?????????
    val pm = system.actorOf(ModuleManager.props("modulemanager", sysName, false, false, false), "modulemanager")
    // ??????????????????
    val s1 = scala.io.Source.fromFile("src/main/scala/rep/sc/tpl/ContractAssetsTPL.scala")
    val l1 = try s1.mkString finally s1.close()
    // ????????????????????????
    val s2 = scala.io.Source.fromFile("src/main/scala/rep/sc/tpl/ContractCert.scala")
    val l2 = try s2.mkString finally s2.close()
    val sm: SetMap = Map("121000005l35120456" -> 50, "12110107bi45jh675g" -> 50, "122000002n00123567" -> 50)
    val sms = write(sm)
    //val aa = new ContractCert
    val tcs = Array(
      Transfer("121000005l35120456", "12110107bi45jh675g", 5),
      Transfer("121000005l35120456", "12110107bi45jh675g0", 5),
      Transfer("121000005l35120456", "12110107bi45jh675g", 500))
    val rcs = Array(None, "?????????????????????", "????????????")

    val signer = Signer("node2", "12110107bi45jh675g", "13856789234", Seq("node2"))
    val cert = scala.io.Source.fromFile("jks/certs/12110107bi45jh675g.node2.cer")
    val certStr = try cert.mkString finally cert.close()
    val certinfo = CertInfo("12110107bi45jh675g", "node2", Certificate(certStr, "SHA1withECDSA", true, None, None))
    //???????????????????????????????????????
    val probe = TestProbe()
    val db = ImpDataAccess.GetDataAccess(sysName)
    val sandbox = system.actorOf(TransactionDispatcher.props("transactiondispatcher"), "transactiondispatcher")

    // ????????????ContractAssetsTPL???????????????ChainCodeIdName????????????????????????????????????????????????????????????
    val cid1 = ChaincodeId(SystemProfile.getAccountChaincodeName, 1)
    val cid2 = ChaincodeId("ContractCert", 1)

    //??????deploy??????
    // ??????????????????
    val t1 = PeerHelper.createTransaction4Deploy(sysName, cid1, l1,
      "", 5000, rep.protos.peer.ChaincodeDeploy.CodeType.CODE_SCALA)

    val msg_send1 = DoTransaction(t1, "dbnumber", TypeOfSender.FromAPI)
    probe.send(sandbox, msg_send1)
    val msg_recv1 = probe.expectMsgType[Sandbox.DoTransactionResult](1000.seconds)
    msg_recv1.err.isEmpty should be(true)

    // ??????????????????
    val t2 = PeerHelper.createTransaction4Deploy(sysName, cid2, l2,
      "", 5000, rep.protos.peer.ChaincodeDeploy.CodeType.CODE_SCALA)
    val msg_send2 = DoTransaction(t2, "dbnumber", TypeOfSender.FromAPI)
    probe.send(sandbox, msg_send2)

    // ??????invoke??????
    // ????????????
    val t3 = PeerHelper.createTransaction4Invoke(sysName, cid2, ACTION.SignUpSigner, Seq(write(signer)))
    val msg_send3 = DoTransaction(t3, "dbnumber", TypeOfSender.FromAPI)
    probe.send(sandbox, msg_send3)

    //deploy??????invoke??????
    val msg_recv2 = probe.expectMsgType[Sandbox.DoTransactionResult](1000.seconds)
    msg_recv2.err.isEmpty should be(true)
    val msg_recv3 = probe.expectMsgType[Sandbox.DoTransactionResult](1000.seconds)
    msg_recv3.err should be(None)

    // ????????????
    val t4 = PeerHelper.createTransaction4Invoke(sysName, cid2, ACTION.SignUpCert, Seq(writePretty(certinfo)))
    val msg_send4 = DoTransaction(t4, "dbnumber", TypeOfSender.FromAPI)
    probe.send(sandbox, msg_send4)
    val msg_recv4 = probe.expectMsgType[Sandbox.DoTransactionResult](1000.seconds)
    msg_recv4.err should be(None)


    //??????invoke??????
    val t5 = PeerHelper.createTransaction4Invoke(sysName, cid1, ACTION.set, Seq(sms))
    val msg_send5 = DoTransaction(t5, "dbnumber", TypeOfSender.FromAPI)
    probe.send(sandbox, msg_send5)
    val msg_recv5 = probe.expectMsgType[Sandbox.DoTransactionResult](1000.seconds)
    msg_recv5.err should be(None)

    for (i <- 0 until tcs.length) {
      val t6 = PeerHelper.createTransaction4Invoke(sysName, cid1, ACTION.transfer, Seq(write(tcs(i))))
      val msg_send6 = DoTransaction(t6, "dbnumber", TypeOfSender.FromAPI)
      probe.send(sandbox, msg_send6)
      val msg_recv6 = probe.expectMsgType[Sandbox.DoTransactionResult](1000.seconds)
      if (msg_recv6.err.isEmpty && i == 0)
        msg_recv6.err should be(rcs(0))
      else
        msg_recv6.err.get.cause.getMessage should be(rcs(i))
    }
  }
}
