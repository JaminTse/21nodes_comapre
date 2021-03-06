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

package rep.app.conf

import java.io._
import rep.utils.IdTool
import rep.crypto.cert.SignTool
import rep.log.RepLogger

/**
 * @author jiangbuyun
 * @version	0.7
 * @category	获取信任的证书列表，抽签时从此文件中获取。
 * */
object SystemCertList {
  private var mySystemCertList:Set[String] = (new scala.collection.mutable.ArrayBuffer[String]()).toSet[String]

  private  def loadVoteNodeListForCert = {
    synchronized{
      if(this.mySystemCertList.isEmpty){
        val list = SystemProfile.getVoteNodeList
        val clist = SignTool.getAliasOfTrustkey
        var rlist : scala.collection.mutable.ArrayBuffer[String] = new scala.collection.mutable.ArrayBuffer[String]()
        for( i <- 0 until clist.size()){
          val alias = clist.get(i)
          if(list.contains(alias)){
            rlist += alias
          }
        }
        this.mySystemCertList = rlist.toSet[String]
        RepLogger.trace(RepLogger.System_Logger, this.mySystemCertList.mkString(","))
      }
    }
  }

  def getSystemCertList:Set[String] = {
    if(this.mySystemCertList.isEmpty){
      loadVoteNodeListForCert
    }
    this.mySystemCertList
  }

}