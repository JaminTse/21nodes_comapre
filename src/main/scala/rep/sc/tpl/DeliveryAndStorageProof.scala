
package rep.sc.tpl

import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods.parse
import org.json4s.jackson.Serialization.write
import rep.protos.peer.ActionResult
import rep.sc.scalax.{ContractContext, IContract, ContractException}
import org.json4s.jvalue2extractable
import org.json4s.string2JsonInput
import scala.reflect.ManifestFactory.classType

final case class  DeliveryAndStorageProof(
                                           fileID: String,
                                           hash: String,
                                           hashAlg: String,
                                           name: String,
                                           size: Int,
                                           uploadTimestamp: BigInt,
                                           storageTimestamp: BigInt,
                                           ownerCreditID: String,
                                           ownerCertName: String,
                                           request: String,
                                           signature: String,
                                           signAlg: String
                                         )


final case class AccessAuthAlteringProof(
                                          fileID: String,
                                          accessAuthID: String,
                                          objectUserID: String,
                                          accessAuthority: Int,
                                          accessRestrictionType: String,
                                          accessRestrictionContent: String,
                                          accessPostProofStorage: Int,
                                          requesterCreditID: String,
                                          requesterCertName: String,
                                          timestamp: BigInt,
                                          request: String,
                                          signature: String,
                                          signAlg: String,
                                        )
final case class AccessAuthAlteringProofWithTxid(
                                                  proof: AccessAuthAlteringProof,
                                                  txid: String
                                                )

final case class DownloadingProof(
                                   fileID: String,
                                   requesterCreditID: String,
                                   requesterCertName: String,
                                   timestamp: BigInt,
                                   request: String,
                                   signature: String,
                                   signAlg: String,
                                 )
final case class DownloadingProofWithTxid(
                                           proof: DownloadingProof,
                                           txid: String
                                         )

class filOpsProSto extends IContract{
  final case class DeliveryAndStorageProofWithTxid(
                                                    proof: DeliveryAndStorageProof,
                                                    txid: String
                                                  )

  val delimiter = "+"
  implicit val formats = DefaultFormats

  def init(ctx: ContractContext){
    println(s"tid: $ctx.t.id")
  }

  /**
   * ??????????????????????????????
   *
   * @param ctx
   * @param data
   * @return
   */
  def fileDeliverAndStore(ctx: ContractContext, data: DeliveryAndStorageProof): ActionResult = {
    val pv:Any = ctx.api.getVal(data.fileID)
    if(pv != null)
      throw ContractException(s"?????????????????????????????? ${data.fileID} ???????????????????????????????????? ???????????? $pv")

    val proofWithTxid = DeliveryAndStorageProofWithTxid(data, ctx.t.id)
    val proofWithTxid2 = new DeliveryAndStorageProofWithTxid(data, ctx.t.id)
    val value = write(proofWithTxid)
    // val value = write(data)
    ctx.api.setVal(data.fileID, value)
    println("Stored proof: "+ data.fileID + ": " + value)

    null
  }

  /**
   * ????????????????????????????????????
   *
   * @param ctx
   * @param data
   * @return
   */
  def fileAccessAuthAlter(ctx: ContractContext, data: AccessAuthAlteringProof): ActionResult = {
    var pv:Any = ctx.api.getVal(ctx.t.id)
    if(pv != null)
      throw ContractException(s"?????????????????????????????? ${ctx.t.id} ???????????????????????????????????????????????? $pv")
    pv = ctx.api.getVal(data.fileID)
    if(pv == null)
      throw ContractException(s"???????????????????????? ${data.fileID} ?????????, ??????????????????????????????????????????")

    // val proofWithTxid = AccessAuthAlteringProofWithTxid(data, ctx.t.id)
    // val value = write(proofWithTxid)
    val value = write(data)
    ctx.api.setVal(ctx.t.id, value)
    println("Stored proof: "+ ctx.t.id + ": " + value)
    null
  }

  /**
   * ????????????????????????
   *
   * @param ctx
   * @param data
   * @return
   */
  def fileDownload(ctx: ContractContext, data: DownloadingProof): ActionResult = {
    var pv:Any = ctx.api.getVal(ctx.t.id)
    if(pv != null)
      throw ContractException(s"?????????????????? ${ctx.t.id} ???????????????????????????????????????????????? $pv")
    pv = ctx.api.getVal(data.fileID)
    if(pv == null)
      throw ContractException(s"???????????????????????? ${data.fileID} ?????????, ??????????????????????????????????????????")

    // val proofWithTxid = DownloadingProofWithTxid(data, ctx.t.id)
    // val value = write(proofWithTxid)
    val value = write(data)
    ctx.api.setVal(ctx.t.id, value)
    println("Stored proof: "+ ctx.t.id + ": " + value)
    null
  }

  def onAction(ctx: ContractContext,action:String, sdata:String ): ActionResult={
    val json = parse(sdata)

    println("-----------"+sdata)
    action match {
      case "fileDeliverAndStore" =>
        fileDeliverAndStore(ctx, json.extract[DeliveryAndStorageProof])
      case "fileAccessAuthAlter" =>
        fileAccessAuthAlter(ctx, json.extract[AccessAuthAlteringProof])
      case "fileDownload" =>
        fileDownload(ctx, json.extract[DownloadingProof])
    }
  }
}