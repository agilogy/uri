package com.agilogy.uri

case class UserInfo(value:String) extends AnyVal{
  override def toString: String = value
}

case class Host(value:String) extends AnyVal{
  override def toString: String = value
}

case class Port(value:Int) extends AnyVal{
  override def toString: String = value.toString
}

case class Authority(userInfo:Option[UserInfo], host:Host, port:Option[Port]){
  override def toString: String = {
    def sUserInfo = userInfo.map(ui => s"${ui.value}@").getOrElse("")
    def sPort = port.map(p => s":$p").getOrElse("")
    s"$sUserInfo$host$sPort"
  }
}

object Authority{

  def apply(host:String): Authority = Authority(userInfo = None, Host(host), port = None)

  def apply(host:String, port:Int):Authority = Authority(userInfo = None, Host(host), Some(Port(port)))

  def apply(userInfo:String, host:String, port:Int):Authority = Authority(Some(UserInfo(userInfo)), Host(host), Some(Port(port)))
}