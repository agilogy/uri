package com.agilogy.uri

import scala.util.{ Failure, Success, Try }

sealed abstract case class UserInfo private (stringValue: String) extends UriPart

object UserInfo {
  def apply(s: String): UserInfo = new UserInfo(Encoder.normalize(s)) {}
}

abstract case class Port private (intValue: Int) {
  val stringValue: String = intValue.toString
  val asciiStringValue: String = stringValue
}

object Port {
  def apply(v: Int): Either[NegativePort, Port] = {
    if (v < 0) Left(NegativePort(v))
    else Right(new Port(v) {})
  }
}

case class Authority(userInfo: Option[UserInfo], host: Host, port: Option[Port]) extends UriPart {

  def stringValue: String = Encoder.quoteAuthority(this)

}

object Authority {

  def apply(host: Host): Authority = Authority(None, host, None)
  def apply(host: String): Authority = Authority(Host(host))
  def apply(userInfo: UserInfo, host: Host): Authority = Authority(Some(userInfo), host, None)
  def apply(userInfo: String, host: String): Authority = Authority(UserInfo(userInfo), Host(host))
  def apply(host: Host, port: Port): Authority = Authority(None, host, Some(port))
  def apply(host: String, port: Port): Authority = Authority(Host(host), port)
  def apply(host: String, port: Int): Either[NegativePort, Authority] = Port(port).map(Authority(Host(host), _))
  def apply(userInfo: UserInfo, host: Host, port: Port): Authority = Authority(Some(userInfo), host, Some(port))
  def apply(userInfo: String, host: String, port: Port): Authority = Authority(Some(UserInfo(userInfo)), Host(host), Some(port))
  def apply(userInfo: String, host: String, port: Int): Either[NegativePort, Authority] = Port(port).map(Authority(UserInfo(userInfo), Host(host), _))

  private val AuthorityRe = "(([^/?#@]*)@)?([^/?#@:]*)(:([0-9]*))?".r

  def parse(s: String): Either[AuthorityParseError, Authority] = {
    s match {
      case AuthorityRe(_, sUserInfo, sHost, _, sPort) =>
        val userInfo = Option(sUserInfo).map(ui => UserInfo(Encoder.decode(ui)))
        val host = Host(Encoder.decode(Option(sHost).getOrElse("")))
        // The regex guarantees the port wont be negative
        val port = Option(sPort).map(p => Port(p.toInt).right.get)
        Right(Authority(userInfo, host, port))
      case _ =>
        //TODO: Test this case
        Left(AuthorityParseError(s))
    }

  }

  def parseTry(s: String): Try[Authority] = parse(s) match {
    case Left(e)  => Failure(AuthorityParseException(e))
    case Right(r) => Success(r)
  }
}
