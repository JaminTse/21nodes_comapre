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

package rep.network.consensus.util

import com.google.protobuf.ByteString
import com.google.protobuf.timestamp.Timestamp
import scalapb.json4s.JsonFormat
import rep.app.conf.SystemProfile
import rep.crypto.{ Sha256 }
import rep.protos.peer.{ Block, Signature, Transaction, ChaincodeId, CertId }
import rep.utils.TimeUtils
import rep.storage.IdxPrefix
import rep.sc.Shim._
import rep.storage._
import java.security.cert.{ Certificate }
import rep.network.PeerHelper
import rep.utils.SerializeUtils
import scala.util.control.Breaks
import org.slf4j.LoggerFactory
import rep.crypto.cert.SignTool
import rep.utils.IdTool

object BlockHelp {
  /****************************背书相关的操作开始**********************************************************/
  def SignDataOfBlock(NonEndorseDataOfBlock: Array[Byte], alise: String): Signature = {
    try {
      val millis = TimeUtils.getCurrentTime()
      val certid = IdTool.getCertIdFromName(alise)
      Signature(
        Option(certid),
        Option(Timestamp(millis / 1000, ((millis % 1000) * 1000000).toInt)),
        ByteString.copyFrom(SignTool.sign4CertId(certid, NonEndorseDataOfBlock)))
    } catch {
      case e: RuntimeException => throw e
    }
  }

  def SignBlock(block: Block, alise: String): Signature = {
    try {
      val tmpblock = block.clearEndorsements
      SignDataOfBlock(tmpblock.toByteArray, alise)
    } catch {
      case e: RuntimeException => throw e
    }
  }

  def AddSignToBlock(block: Block, alise: String): Block = {
    try {
      var signdata = SignBlock(block, alise)
      AddEndorsementToBlock(block, signdata)
    } catch {
      case e: RuntimeException => throw e
    }
  }

  def AddEndorsementToBlock(block: Block, signdata: Signature): Block = {
    try {
      if (block.endorsements.isEmpty) {
        block.withEndorsements(Seq(signdata))
      } else {
        block.withEndorsements(block.endorsements.+:(signdata))
      }
    } catch {
      case e: RuntimeException => throw e
    }
  }


  /****************************背书相关的操作结束**********************************************************/

  //该方法在预执行结束之后才能调用
  def AddBlockHash(block: Block): Block = {
    try {
      block.withHashOfBlock(ByteString.copyFromUtf8(GetBlockHash(block)))
    } catch {
      case e: RuntimeException => throw e
    }
  }

  def GetBlockHash(block: Block): String = {
    try {
      val blkOutEndorse = block.clearEndorsements
      val blkOutBlockHash = blkOutEndorse.withHashOfBlock(ByteString.EMPTY)
      Sha256.hashstr(blkOutBlockHash.toByteArray)
    } catch {
      case e: RuntimeException => throw e
    }
  }

  //打包交易到区块，等待预执行
  def WaitingForExecutionOfBlock(preBlockHash: String, h: Long, trans: Seq[Transaction]): Block = {
    try {
      val millis = TimeUtils.getCurrentTime()
      new Block(
        1,
        h,
        trans,
        null,
        _root_.com.google.protobuf.ByteString.EMPTY,
        ByteString.copyFromUtf8(preBlockHash),
        Seq(),
        _root_.com.google.protobuf.ByteString.EMPTY)
    } catch {
      case e: RuntimeException => throw e
    }
  }

  def CreateGenesisBlock:Block={
    val blkJson = scala.io.Source.fromFile("json/gensis.json","UTF-8")
    val blkStr = try blkJson.mkString finally blkJson.close()
    val gen_blk = JsonFormat.fromJsonString[Block](blkStr)
    gen_blk
  }

}