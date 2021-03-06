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

package rep.network.consensus.block

import akka.util.Timeout
import scala.concurrent.duration._
import akka.pattern.ask
import akka.pattern.AskTimeoutException
import scala.concurrent._
import rep.app.conf.{ SystemProfile }
import akka.actor.{ ActorRef, Props, Address }
import rep.crypto.Sha256
import rep.network.base.ModuleBase
import rep.network.Topic
import rep.network.util.NodeHelp
import rep.protos.peer.{ Event, Transaction }
import rep.utils.GlobalUtils.{ ActorType, BlockEvent, EventType, NodeStatus }
import com.sun.beans.decoder.FalseElementHandler
import scala.util.control.Breaks._
import scala.util.control.Exception.Finally
import java.util.concurrent.ConcurrentHashMap
import rep.network.consensus.block.Blocker.{ ConfirmedBlock }
import rep.network.persistence.Storager.{ BlockRestore, SourceOfBlock, BatchStore }
import rep.network.consensus.util.{ BlockVerify, BlockHelp }
import rep.log.RepLogger
import rep.log.RepTimeTracer
import rep.network.sync.SyncMsg.SyncRequestOfStorager
import rep.network.consensus.vote.Voter.VoteOfBlocker

object ConfirmOfBlock {
  def props(name: String): Props = Props(classOf[ConfirmOfBlock], name)
}

class ConfirmOfBlock(moduleName: String) extends ModuleBase(moduleName) {
  import context.dispatcher

  override def preStart(): Unit = {
    RepLogger.info(RepLogger.Consensus_Logger, this.getLogMsgPrefix("confirm Block module start"))
    SubscribeTopic(mediator, self, selfAddr, Topic.Block, false)
  }
  import scala.concurrent.duration._
  import rep.protos.peer._

  implicit val timeout = Timeout(3 seconds)

  private def asyncVerifyEndorse(e: Signature, byteOfBlock: Array[Byte]): Future[Boolean] = {
    val result = Promise[Boolean]

    val tmp = BlockVerify.VerifyOneEndorseOfBlock(e, byteOfBlock, pe.getSysTag)
    if (tmp._1) {
      result.success(true)
    } else {
      result.success(false)
    }
    result.future
  }

  private def asyncVerifyEndorses(block: Block): Boolean = {
    val b = block.clearEndorsements.toByteArray
    val listOfFuture: Seq[Future[Boolean]] = block.endorsements.map(x => {
      asyncVerifyEndorse(x, b)
    })
    val futureOfList: Future[List[Boolean]] = Future.sequence(listOfFuture.toList).recover({
      case e: Exception =>
        null
    })

    val result1 = Await.result(futureOfList, timeout.duration).asInstanceOf[List[Boolean]]

    var result = true
    if (result1 == null) {
      false
    } else {
      result1.foreach(f => {
        if (!f) {
          result = false
          RepLogger.trace(RepLogger.Consensus_Logger, this.getLogMsgPrefix(s"comfirmOfBlock verify endorse is error, break,block height=${block.height},local height=${pe.getCurrentHeight}"))
        }
      })
    }

    result
  }

  private def handler(block: Block, actRefOfBlock: ActorRef) = {
    RepLogger.trace(RepLogger.Consensus_Logger, this.getLogMsgPrefix(s"confirm verify endorsement start,height=${block.height}"))
    if (SystemProfile.getIsVerifyOfEndorsement) {
      if (asyncVerifyEndorses(block)) {
        RepLogger.trace(RepLogger.Consensus_Logger, this.getLogMsgPrefix(s"confirm verify endorsement end,height=${block.height}"))
        //????????????????????????
        if (BlockVerify.verifySort(block.endorsements.toArray[Signature]) == 1 || (block.height == 1 && pe.getCurrentBlockHash == "" && block.previousBlockHash.isEmpty())) {
          //????????????????????????
          RepLogger.trace(RepLogger.Consensus_Logger, this.getLogMsgPrefix(s"confirm verify endorsement sort,height=${block.height}"))
          pe.getBlockCacheMgr.addToCache(BlockRestore(block, SourceOfBlock.CONFIRMED_BLOCK, actRefOfBlock))
          pe.getActorRef(ActorType.storager) ! BatchStore
          sendEvent(EventType.RECEIVE_INFO, mediator, pe.getSysTag, Topic.Block, Event.Action.BLOCK_NEW)
        } else {
          ////????????????????????????
        }
      } else {
        //?????????????????????
      }
    } else {
      RepLogger.trace(RepLogger.Consensus_Logger, this.getLogMsgPrefix(s"confirm verify endorsement sort,height=${block.height}"))
      pe.getBlockCacheMgr.addToCache(BlockRestore(block, SourceOfBlock.CONFIRMED_BLOCK, actRefOfBlock))
      pe.getActorRef(ActorType.storager) ! BatchStore
      sendEvent(EventType.RECEIVE_INFO, mediator, pe.getSysTag, Topic.Block, Event.Action.BLOCK_NEW)
    }
  }

  private def checkedOfConfirmBlock(block: Block, actRefOfBlock: ActorRef) = {
    if (pe.getCurrentBlockHash == "" && block.previousBlockHash.isEmpty()) {
      RepLogger.trace(RepLogger.Consensus_Logger, this.getLogMsgPrefix(s"confirm verify blockhash,height=${block.height}"))
      handler(block, actRefOfBlock)
    } else {
      //?????????????????????
      RepLogger.trace(RepLogger.Consensus_Logger, this.getLogMsgPrefix(s"confirm verify blockhash,height=${block.height}"))
      if (SystemProfile.getNumberOfEndorsement == 1) {
        /*if (block.height > pe.getCurrentHeight + 1) {
          RepLogger.trace(RepLogger.Consensus_Logger, this.getLogMsgPrefix(s"confirm verify height,height=${block.height}???localheight=${pe.getCurrentHeight }"))
          //pe.getActorRef(ActorType.synchrequester) ! StartSync(false)
          pe.getActorRef(ActorType.synchrequester) ! SyncRequestOfStorager(sender,block.height)
        } else {*/
        handler(block, actRefOfBlock)
        pe.setConfirmHeight(block.height)
        //}
        //pe.getActorRef(ActorType.voter) ! VoteOfBlocker
      } else {
        if (NodeHelp.ConsensusConditionChecked(block.endorsements.size, pe.getNodeMgr.getStableNodes.size)) {
          //??????????????????????????????
          handler(block, actRefOfBlock)
        } else {
          //?????????????????????????????????????????????
        }
      }
    }
  }

  override def receive = {
    //Endorsement block
    case ConfirmedBlock(block, actRefOfBlock) =>
      RepTimeTracer.setStartTime(pe.getSysTag, "blockconfirm", System.currentTimeMillis(), block.height, block.transactions.size)
      checkedOfConfirmBlock(block, actRefOfBlock)
      RepTimeTracer.setEndTime(pe.getSysTag, "blockconfirm", System.currentTimeMillis(), block.height, block.transactions.size)
    case _ => //ignore
  }

}