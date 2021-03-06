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

package rep.api.rest

import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout
import com.google.protobuf.ByteString
import org.json4s._
import org.json4s.jackson.JsonMethods
import rep.app.TestMain
import rep.app.conf.SystemProfile
import rep.crypto._
import rep.crypto.cert.SignTool
import rep.log.RepLogger
import rep.network._
import rep.network.base.ModuleBase
import rep.protos.peer.{ActionResult, _}
import rep.sc.Sandbox.DoTransactionResult
import rep.sc.SandboxDispatcher.DoTransaction
import rep.sc.TypeOfSender
import rep.storage._
import rep.utils.GlobalUtils.ActorType
import rep.utils.SerializeUtils
import scalapb.json4s.JsonFormat

import scala.concurrent._
import scala.concurrent.duration._

/**
 * RestActor伴生object，包含可接受的传入消息定义，以及处理的返回结果定义。
 * 以及用于建立Tranaction，检索Tranaction的静态方法
 *
 * @author c4w created
 *
 */

object RestActor {
  def props(name: String): Props = Props(classOf[RestActor], name)

  val contractOperationMode = SystemProfile.getContractOperationMode

  case object ChainInfo

  case object NodeNumber

  case object TransNumber

  case object AcceptedTransNumber

  case class SystemStart(cout: Int)

  case class SystemStop(from: Int, to: Int)

  case class BlockId(bid: String)

  case class BlockHeight(h: Int)

  case class BlockTime(createTime: String, createTimeUtc: String)

  case class BlockTimeForHeight(h: Long)

  case class BlockTimeForTxid(txid: String)

  case class BlockHeightStream(h: Int)

  case class TransactionId(txid: String)

  case class TransactionStreamId(txid: String)

  case class TransNumberOfBlock(height: Long)

  case object LoadBlockInfo

  case object IsLoadBlockInfo

  case class PostResult(txid: String, result: Option[ActionResult], err: Option[String])

  case class QueryResult(result: Option[JValue])

  case class resultMsg(result: String)

  /*case class CSpec(stype: Int, idPath: String, idName: Option[String],
          iptFunc: String, iptArgs: Seq[String], timeout: Int,
          secureContext: String, code: String, ctype: Int)    */
  case class CSpec(stype: Int, chaincodename: String, chaincodeversion: Int,
                   iptFunc: String, iptArgs: Seq[String], timeout: Int, legal_prose: String,
                   code: String, ctype: Int, state: Boolean)

  case class tranSign(tran: String)

  case class TranInfoAndHeightId(txid: String)

  case class TranInfoHeight(tranInfo: JValue, height: Long)

  /**
   * 根据节点名称和chainCode定义建立交易实例
   *
   * @param nodeName 节点名称
   * @param c        chainCode定义
   */
  def buildTranaction(nodeName: String, c: CSpec): Transaction = {
    val stype = c.stype match {
      case 1 =>
        Transaction.Type.CHAINCODE_DEPLOY
      case 2 =>
        Transaction.Type.CHAINCODE_INVOKE
      case 3 =>
        Transaction.Type.CHAINCODE_SET_STATE
      case _ =>
        Transaction.Type.UNDEFINED
    }
    val ctype = c.ctype match {
      case 2 =>
        rep.protos.peer.ChaincodeDeploy.CodeType.CODE_SCALA
      case 3 =>
        rep.protos.peer.ChaincodeDeploy.CodeType.CODE_SCALA_PARALLEL
      case _ =>
        rep.protos.peer.ChaincodeDeploy.CodeType.CODE_JAVASCRIPT
    }

    val chaincodeId = new ChaincodeId(c.chaincodename, c.chaincodeversion)
    if (stype == Transaction.Type.CHAINCODE_DEPLOY) {
      PeerHelper.createTransaction4Deploy(nodeName, chaincodeId, c.code, c.legal_prose, c.timeout, ctype)
    } else if (stype == Transaction.Type.CHAINCODE_INVOKE) {
      PeerHelper.createTransaction4Invoke(nodeName, chaincodeId, c.iptFunc, c.iptArgs)
    } else if (stype == Transaction.Type.CHAINCODE_SET_STATE) {
      PeerHelper.createTransaction4State(nodeName, chaincodeId, c.state)
    } else {
      null
    }
  }

}

/**
 * RestActor负责处理rest api请求
 *
 */
class RestActor(moduleName: String) extends ModuleBase(moduleName) {

  import RestActor._
  import akka.http.scaladsl.model.{HttpEntity, HttpResponse, MediaTypes}

  //import rep.utils.JsonFormat.AnyJsonFormat

  implicit val timeout = Timeout(1000.seconds)
  val sr: ImpDataAccess = ImpDataAccess.GetDataAccess(pe.getSysTag)

  // 先检查交易大小，然后再检查交易是否已存在，再去验证签名，如果没有问题，则广播
  def preTransaction(t: Transaction): Unit = {
    val tranLimitSize = SystemProfile.getBlockLength / 3
    if (t.toByteArray.length > tranLimitSize) {
      sender ! PostResult(t.id, None, Option(s"交易大小超出限制： ${tranLimitSize}，请重新检查"))
    }

    if (pe.getNodeMgr.getStableNodes.size < SystemProfile.getVoteNoteMin) {
      sender ! PostResult(t.id, None, Option("共识节点数目太少，暂时无法处理交易"))
    }

    /*if (pe.getTransPoolMgr.findTrans(t.id) || sr.isExistTrans4Txid(t.id)) {
          sender ! PostResult(t.id, None, Option(s"transactionId is exists, the transaction is \n ${t.id}"))
    }*/

    try {
      if (SystemProfile.getHasPreloadTransOfApi) {
        val sig = t.signature.get.signature.toByteArray
        val tOutSig = t.clearSignature
        val certId = t.signature.get.certId.get
        if (pe.getTransPoolMgr.findTrans(t.id) || sr.isExistTrans4Txid(t.id)) {
          sender ! PostResult(t.id, None, Option(s"transactionId is exists, the transaction is \n ${t.id}"))
        } else {
          val isTrue = SignTool.verify(sig, tOutSig.toByteArray, certId, pe.getSysTag)
          if (isTrue) {
            //            RepLogger.info(RepLogger.Business_Logger, s"验证签名成功，txid: ${t.id},creditCode: ${t.signature.get.getCertId.creditCode}, certName: ${t.signature.get.getCertId.certName}")
            val future = pe.getActorRef(ActorType.transactiondispatcher) ? DoTransaction(t, "api_" + t.id, TypeOfSender.FromAPI)
            val result = Await.result(future, timeout.duration).asInstanceOf[DoTransactionResult]
            val rv = result
            // 释放存储实例

            rv.err match {
              case None =>
                //预执行正常,提交并广播交易
                pe.getActorRef(ActorType.transactionpool) ! t // 给交易池发送消息 ！=》告知（getActorRef）
                if (rv.r == null)
                  sender ! PostResult(t.id, None, None)
                else
                  sender ! PostResult(t.id, Some(rv.r), None) // legal_prose need
              case Some(err) =>
                //预执行异常,废弃交易，向api调用者发送异常
                sender ! PostResult(t.id, None, Option(err.cause.getMessage))
            }
          } else {
            sender ! PostResult(t.id, None, Option("验证签名出错"))
          }
        }
      } else {
        pe.getActorRef(ActorType.transactionpool) ! t // 给交易池发送消息 ！=》告知（getActorRef）
        sender ! PostResult(t.id, None, None)
      }

    } catch {
      case e: RuntimeException =>
        sender ! PostResult(t.id, None, Option(e.getMessage))
    } finally {
      ImpDataPreloadMgr.Free(pe.getSysTag, "api_" + t.id)
    }
  }

  def receive: Receive = {

    case tranSign(tr: String) =>
      val tmpstart = System.currentTimeMillis()
      val tr1 = BytesHex.hex2bytes(tr) // 解析交易编码后的16进制字符串,进行解码16进制反解码decode
      var txr = Transaction.defaultInstance
      try {
        txr = Transaction.parseFrom(tr1)
        val creditCode = txr.getSignature.getCertId.creditCode
        val name = txr.getSignature.getCertId.certName
        val deployer = creditCode + "." + name
        val deployers = "1acd7866562e47c7bc.deployer1;4bd7f7af7c5f44e797.deployer2;bd481390b542432b9b.deployer3"
        if (txr.`type` == Transaction.Type.CHAINCODE_DEPLOY){
          if (!deployers.contains(deployer)){
            sender ! PostResult(txr.id, None, Option("该用户无部署合约权限！"))
          }
        }
        preTransaction(txr)
      } catch {
        case e: Exception =>
          sender ! PostResult(txr.id, None, Option(s"transaction parser error! + ${e.getMessage}"))
      }
      val tmpend = System.currentTimeMillis()
      RepLogger.trace(RepLogger.OutputTime_Logger, this.getLogMsgPrefix(s"API recv trans time,thread-id=${Thread.currentThread().getName + "-" + Thread.currentThread().getId},spent time=${(tmpend - tmpstart)}" + "~" + selfAddr))

    //处理post CSpec构造交易的请求
    case c: CSpec =>
      var txr = Transaction.defaultInstance
      //debug状态才动用节点密钥签名
      if (contractOperationMode == 0) {
        //构建transaction并通过peer预执行广播
        txr = buildTranaction(pe.getSysTag, c)
        preTransaction(txr)
      } else
        sender ! PostResult(txr.id, None, Option("非Debug状态下此调用无效"))

    // 流式提交交易
    case t: Transaction =>
      preTransaction(t)

    case SystemStart(cout) =>
      val rs = TestMain.startSystem(cout)
      val r = rs match {
        case null => QueryResult(None)
        case _ =>
          QueryResult(Option(JsonMethods.parse(string2JsonInput(rs))))
      }
      sender ! r

    case SystemStop(from, to) =>
      val rs = TestMain.stopSystem(from, to)
      val r = rs match {
        case null => QueryResult(None)
        case _ =>
          QueryResult(Option(JsonMethods.parse(string2JsonInput(rs))))
      }
      sender ! r

    case BlockTimeForHeight(h) =>
      val bb = sr.getBlockTimeOfHeight(h)
      val r = bb match {
        case null => QueryResult(None)
        case _ =>
          QueryResult(Option(JsonMethods.parse(bb)))
      }
      sender ! r

    case BlockTimeForTxid(txid) =>
      val bb = sr.getBlockTimeOfTxid(txid)
      val r = bb match {
        case null => QueryResult(None)
        case _ =>
          QueryResult(Option(JsonMethods.parse(bb)))
      }
      sender ! r

    // 根据高度检索块
    case BlockHeight(h) =>
      val bb = sr.getBlockByHeight(h)
      val r = bb match {
        case null => QueryResult(None)
        case _ =>
          val bl = screenBlock(bb)
          //val bl = Block.parseFrom(bb)
          QueryResult(Option(JsonFormat.toJson(bl)))
      }
      sender ! r

    // 根据高度检索块的子节流
    case BlockHeightStream(h) =>
      val bb = sr.getBlockByHeight(h)
      if (bb == null) {
        sender ! HttpResponse(entity = HttpEntity.Strict(MediaTypes.`application/octet-stream`, akka.util.ByteString.empty))
      } else {
        val bl = screenBlock(bb)
        val body = akka.util.ByteString(bl.toByteArray)
        val entity = HttpEntity.Strict(MediaTypes.`application/octet-stream`, body)
        val httpResponse = HttpResponse(entity = entity)
        sender ! httpResponse
      }

    //根据block hash检索
    case BlockId(bid) =>
      val bb = sr.getBlockByBase64Hash(bid)
      val r = bb match {
        case null => QueryResult(None)
        case _ =>
          val bl = screenBlock(bb)
          //val bl = Block.parseFrom(bb)
          QueryResult(Option(JsonFormat.toJson(bl)))
      }
      sender ! r

    // 根据txid检索交易
    case TransactionId(txId) =>
      //直接去掉非法交易
      /*var r = QueryResult(None)
      if (!checkTxid(txId)) {
        r = sr.getTransDataByTxId(txId) match {
          case None =>
            QueryResult(None)
          case t: Some[Transaction] =>
            QueryResult(Option(JsonFormat.toJson(t.get)))
        }
      }
      sender ! r*/
      val r = sr.getTransDataByTxId(txId) match {
        case None =>
          QueryResult(None)
        case t: Some[Transaction] =>
          if (checkTxid(txId)) {
            val nt = screenTransaction(t.get)
            QueryResult(Option(JsonFormat.toJson(nt)))
          } else {
            QueryResult(Option(JsonFormat.toJson(t.get)))
          }
      }
      sender ! r

    // 根据txid检索交易字节流
    case TransactionStreamId(txId) =>
      if (checkTxid(txId)) { //屏蔽不允许访问的交易
        sender ! HttpResponse(entity = HttpEntity.Strict(MediaTypes.`application/octet-stream`, akka.util.ByteString.empty))
      } else {
        val r = sr.getTransDataByTxId(txId)
        if (r.isEmpty) {
          sender ! HttpResponse(entity = HttpEntity.Strict(MediaTypes.`application/octet-stream`, akka.util.ByteString.empty))
        } else {
          val t = r.get
          val body = akka.util.ByteString(t.toByteArray)
          val entity = HttpEntity.Strict(MediaTypes.`application/octet-stream`, body)
          val httpResponse = HttpResponse(entity = entity)
          sender ! httpResponse
        }
      }

    // 获取链信息
    case ChainInfo =>
      val cij = JsonFormat.toJson(sr.getBlockChainInfo)
      sender ! QueryResult(Option(cij))

    case NodeNumber =>
      val stablenode = pe.getNodeMgr.getStableNodes.size
      val snode = pe.getNodeMgr.getNodes.size
      val rs = "{\"consensusnodes\":\"" + stablenode + "\",\"nodes\":\"" + snode + "\"}"
      sender ! QueryResult(Option(JsonMethods.parse(string2JsonInput(rs))))

    case TransNumber =>
      val num = pe.getTransPoolMgr.getTransLength()
      val rs = "{\"numberofcache\":\"" + num + "\"}"
      sender ! QueryResult(Option(JsonMethods.parse(string2JsonInput(rs))))

    case AcceptedTransNumber =>
      val num = sr.getBlockChainInfo.totalTransactions + pe.getTransPoolMgr.getTransLength()
      val rs = "{\"acceptedNumber\":\"" + num + "\"}"
      sender ! QueryResult(Option(JsonMethods.parse(string2JsonInput(rs))))

    case TransNumberOfBlock(h) =>
      val num = sr.getNumberOfTransInBlockByHeight(h)
      val rs = "{\"transnumberofblock\":\"" + num + "\"}"
      sender ! QueryResult(Option(JsonMethods.parse(string2JsonInput(rs))))

    case LoadBlockInfo =>
      sr.loadBlockInfoToCache
      val rs = "{\"startup\":\"" + "true" + "\"}"
      sender ! QueryResult(Option(JsonMethods.parse(string2JsonInput(rs))))
    case IsLoadBlockInfo =>
      val num = sr.isFinish
      val rs = "{\"isfinish\":\"" + num + "\"}"
      sender ! QueryResult(Option(JsonMethods.parse(string2JsonInput(rs))))

    case TranInfoAndHeightId(txId) =>
      implicit val fomats = DefaultFormats
      val r = sr.getTransDataByTxId(txId) match {
        case None =>
          QueryResult(None)
        case t: Some[Transaction] =>
          val txr = t.get
          val tranInfoHeight = TranInfoHeight(JsonFormat.toJson(txr), sr.getBlockIdxByTxid(txr.id).getBlockHeight())
          QueryResult(Option(Extraction.decompose(tranInfoHeight)))
      }
      sender ! r
  }

  private def checkTxid(txid: String): Boolean = {
    val txidSet = getInvalidTxid()
    txidSet.contains(txid)
  }

  private def getInvalidTxid(): Set[String] = {
    val key = GlobalId.ILLEGAL_TRANSACTION_LIST_KEY
    val v = sr.getInvalidTxid(GlobalId.TransactionReview + key)
    var txidSet = SerializeUtils.deserialise(v).asInstanceOf[Set[String]]
    if (txidSet == null) {
      txidSet = Set.empty
    }
    txidSet
  }

  private def screenBlock(blockBytes: Array[Byte]): Block = {
    val txidSet = getInvalidTxid()
    val block = Block.parseFrom(blockBytes)
    val blockWithoutTrans = block.clearTransactions.clearTransactionResults
    val transSet: scala.collection.mutable.Set[Transaction] = scala.collection.mutable.Set.empty
    val transResultSet: scala.collection.mutable.Set[TransactionResult] = scala.collection.mutable.Set.empty

    /** ***************begin 直接去掉整个非法交易*************** */
    /*for (t: Transaction <- block.transactions) {
      if (!txidSet.contains(t.id)) {
        transSet.add(t)
      }
    }
    for (tr: TransactionResult <- block.transactionResults) {
      if (!txidSet.contains(tr.txId)) {
        transResultSet.add(tr)
      }
    }*/
    /** ***************end 直接去掉整个非法交易*************** */

    for (t: Transaction <- block.transactions) {
      if (txidSet.contains(t.id)) {
        val nt = screenTransaction(t)
        transSet.add(nt)
      } else {
        transSet.add(t)
      }
    }
    for (tr: TransactionResult <- block.transactionResults) {
      if (txidSet.contains(tr.txId)) {
        val ntr = screenTransactionResult(tr)
        transResultSet.add(ntr)
      } else {
        transResultSet.add(tr)
      }
    }
    blockWithoutTrans.withTransactions(transSet.toSeq).withTransactionResults(transResultSet.toSeq)
  }

  private def screenTransaction(t: Transaction): Transaction = {
    val tWithoutParam = t.clearPara
    val cipWithoutArgs = t.getIpt.clearArgs
    val cip = cipWithoutArgs.withArgs(Seq("******************"))
    tWithoutParam.withIpt(cip)
  }

  private def screenTransactionResult(tr: TransactionResult): TransactionResult = {
    val trWithoutOl = tr.clearOl
    trWithoutOl.withOl(Seq(OperLog("************",
      ByteString.copyFrom("******", "utf-8"), ByteString.copyFrom("******", "utf-8"))))
  }
}