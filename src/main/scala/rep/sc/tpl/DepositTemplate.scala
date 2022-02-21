package rep.sc.tpl

import org.apache.commons.lang3.StringUtils
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods.parse
import rep.app.conf.SystemProfile
import rep.protos.peer.ActionResult
import rep.sc.scalax.{ContractContext, ContractException, IContract}

/**
 * @Description: 存证模板合约
 * @Author: daiyongbing
 * @CreateDate: 2021/08/19
 * @Version: 1.0
 */
final case class Proof(uniqueId: String, content: String)

class DepositTemplate extends IContract {
  implicit val formats: DefaultFormats.type = DefaultFormats
  val chaincodeName: String = SystemProfile.getAccountChaincodeName
  val chaincodeVersion: Int = SystemProfile.getAccountChaincodeVersion

  def init(ctx: ContractContext) {
    println(s"${ctx.t.getCid.chaincodeName}:${ctx.t.getCid.version}")
  }

  def deposit(ctx: ContractContext, proof: Proof): ActionResult = {
    val uniqueId: String = proof.uniqueId
    val content = proof.content
    if (StringUtils.isBlank(uniqueId) || StringUtils.isBlank(content)) {
      throw ContractException("uniqueId或content为空！")
    }
    val oldValue = ctx.api.getVal(uniqueId)
    if (oldValue != null && oldValue != None) {
      throw ContractException("uniqueId已存在！")
    }
    ctx.api.setVal(uniqueId, content)
    ActionResult(200, "上链请求已受理！")
  }

  def update(ctx: ContractContext, proof: Proof): ActionResult = {
    val uniqueId: String = proof.uniqueId
    val content = proof.content
    if (StringUtils.isBlank(uniqueId) || StringUtils.isBlank(content)) {
      throw ContractException("uniqueId或content为空！")
    }
    ctx.api.setVal(uniqueId, content)
    ActionResult(200, s"更新请求已受理！")
  }

  def getContent(ctx: ContractContext, uniqueId: String): ActionResult = {
    val content = ctx.api.getVal(uniqueId)
    ActionResult(200, s"$content")
  }

  def onAction(ctx: ContractContext, action: String, sdata: String): ActionResult = {
    val json = parse(sdata)
    action match {
      case "deposit" =>
        deposit(ctx, json.extract[Proof])

      case "update" =>
        update(ctx, json.extract[Proof])

      case "getContent" =>
        getContent(ctx, json.extract[String])
    }
  }
}