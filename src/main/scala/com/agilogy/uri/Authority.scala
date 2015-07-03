package com.agilogy.uri

import com.agilogy.uri.Encoder._

case class UserInfo(value:String) extends AnyVal{

  def encoded: String = {
    import Encoder._
    value.pctEncode(userInfo2Encode)
  }

  override def toString: String = value
}

case class Host(value:String) extends AnyVal{
  def encoded:String = value
  override def toString: String = value
}

case class Port(value:Int) extends AnyVal{
  override def toString: String = value.toString
}

case class Authority(userInfo:Option[UserInfo], host:Host, port:Option[Port]){

  lazy val encoded: String = {
    import Encoder._
    def sUserInfo = userInfo.map(ui => s"${ui.encoded}@").getOrElse("")
    def sPort = port.map(p => s":$p").getOrElse("")
    val sHost = host.encoded
    s"$sUserInfo$sHost$sPort"
  }
  override def toString:String = encoded
}

object Authority{

  def apply(host:String): Authority = Authority(userInfo = None, Host(host), port = None)

  def apply(host:String, port:Int):Authority = Authority(userInfo = None, Host(host), Some(Port(port)))

  def apply(userInfo:String, host:String, port:Int):Authority = Authority(Some(UserInfo(userInfo)), Host(host), Some(Port(port)))
}