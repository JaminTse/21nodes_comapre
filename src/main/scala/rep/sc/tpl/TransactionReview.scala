package rep.sc.tpl

import rep.app.conf.SystemProfile
import rep.sc.scalax.{ContractContext, ContractException, IContract}
import rep.utils.SerializeUtils

/**
 * @Description: 非法交易记录、撤销、查询
 * @Author: daiyongbing
 * @CreateDate: 2022/2/7
 * @Version: 1.0
 */

class TransactionReview extends IContract {
  implicit val formats: DefaultFormats.type = DefaultFormats
  val chaincodeName: String = SystemProfile.getAccountChaincodeName
  val chaincodeVersion: Int = SystemProfile.getAccountChaincodeVersion

  val supervisors = "bc33470bf3714934b6.supervisor1;8a225e08262a4d24b2.supervisor2;0ace920324094eec8a.supervisor3"

  def init(ctx: ContractContext) {
    println(s"${ctx.t.getCid.chaincodeName}:${ctx.t.getCid.version}")
  }

  val key: String = GlobalId.ILLEGAL_TRANSACTION_LIST_KEY

  def isAuthorized(ctx: ContractContext): Boolean = {
    var isAuthorized = false
    val creditCode = ctx.t.getSignature.getCertId.creditCode
    val name = ctx.t.getSignature.getCertId.certName
    val supervisor = creditCode + "." + name
    if (supervisors.contains(supervisor)) {
      isAuthorized = true
    }
    isAuthorized
  }

  def record(ctx: ContractContext, txids: Set[String]): ActionResult = {
    if (!isAuthorized(ctx)) {
      throw ContractException("该用户无监管权限！")
    }
    try {
      val oldSet: Set[String] = ctx.api.getVal(key).asInstanceOf[Set[String]]
      if (oldSet == null || oldSet == None) {
        ctx.api.setVal(key, txids)
      } else {
        val newSet = oldSet.++(txids)
        ctx.api.setVal(key, newSet)
      }
      ActionResult(200, "记录请求已受理！")
    } catch {
      case exception: Exception =>
        throw ContractException(exception.getMessage)
    }
  }

  def cancel(ctx: ContractContext, txids: Set[String]): ActionResult = {
    if (!isAuthorized(ctx)) {
      throw ContractException("该用户无监管权限！")
    }
    try {
      val oldSet: Set[String] = ctx.api.getVal(key).asInstanceOf[Set[String]]
      if (oldSet == null || oldSet == None) {
        throw ContractException("已无可撤销记录！")
      } else {
        val newSet = oldSet.--(txids)
        ctx.api.setVal(key, newSet)
      }
      ActionResult(200, "撤销请求已受理！")
    } catch {
      case exception: Exception =>
        throw ContractException(exception.getMessage)
    }
  }

  def getInvalidTxid(ctx: ContractContext): ActionResult = {
    if (!isAuthorized(ctx)) {
      throw ContractException("该用户无监管权限！")
    }
    val content = ctx.api.getVal(key)
    val jsonStr = SerializeUtils.compactJson(content)
    ActionResult(200, s"$jsonStr")
  }

  def onAction(ctx: ContractContext, action: String, sdata: String): ActionResult = {
    val json = parse(sdata)
    action match {
      case "record" =>
        record(ctx, json.extract[Set[String]])

      case "cancel" =>
        cancel(ctx, json.extract[Set[String]])

      case "getInvalidTxid" =>
        getInvalidTxid(ctx)
    }
  }
}