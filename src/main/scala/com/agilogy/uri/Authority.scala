package com.agilogy.uri

case class UserInfo(value:String) extends AnyVal

case class Host(value:String) extends AnyVal

case class Port(value:Int) extends AnyVal

case class Authority(userInfo:Option[UserInfo], host:Host, port:Option[Port])