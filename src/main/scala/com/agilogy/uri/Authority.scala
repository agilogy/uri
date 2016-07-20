package com.agilogy.uri

import scala.util.{Failure, Success, Try}

abstract case class UserInfo private (stringValue:String) {
  override def toString: String = s"""UserInfo("$stringValue")"""
  def asciiStringValue = Encoder.asciiEncode(stringValue)
}

object UserInfo{
  def apply(s:String):UserInfo = new UserInfo(Encoder.normalize(s)){}
}

abstract case class Port private (intValue:Int){
  val stringValue:String = intValue.toString
  val asciiStringValue:String = stringValue
}

object Port{
  def apply(v:Int):Port = {
    require(v >= 0)
    new Port(v){}
  }
}

case class Authority(userInfo: Option[UserInfo], host: Host, port: Option[Port]) {

  def stringValue:String = Encoder.quoteAuthority(this)
  def asciiStringValue:String = Encoder.asciiEncode(stringValue)

}

object Authority{

  def apply(host:Host):Authority = Authority(None, host, None)
  def apply(host:String):Authority = Authority(RegisteredName(host))
  def apply(userInfo: UserInfo, host:Host):Authority = Authority(Some(userInfo), host, None)
  def apply(userInfo: String, host:String):Authority = Authority(UserInfo(userInfo),RegisteredName(host))
  def apply(host:Host, port: Port):Authority = Authority(None, host, Some(port))
  def apply(host:String, port: Int):Authority = Authority(RegisteredName(host),Port(port))
  def apply(userInfo: UserInfo, host:Host, port: Port):Authority = Authority(Some(userInfo), host, Some(port))
  def apply(userInfo: String, host:String, port: Int):Authority = Authority(UserInfo(userInfo),RegisteredName(host),Port(port))

  private val AuthorityRe = "(([^/?#@]*)@)?([^/?#@:]*)(:([0-9]*))?".r

  def parse(s:String):Try[Authority] = {
    s match {
      case AuthorityRe(_,sUserInfo,sHost,_,sPort) =>
        val userInfo = Option(sUserInfo).map(ui => UserInfo(Encoder.decode(ui)))
        val host = RegisteredName(Encoder.decode(Option(sHost).getOrElse("")))
        val port = Option(sPort).map(p => Port(p.toInt))
        Success(Authority(userInfo,host,port))
      case _ =>
        Failure(new IllegalArgumentException(s"Illegal authority: $s"))
    }
  }
}
