package com.agilogy.uri

import java.text.ParseException

import scala.util.{Failure, Success, Try}

sealed trait UriReference

trait Uri extends UriReference {

  def scheme: Scheme

  def authority: Option[Authority]

  def path: Option[Path]

  def query: Option[Query]

  def fragment: Option[Fragment]

  def stringValue:String = Encoder.quoteUri(this)

  def asciiStringValue:String = Encoder.asciiEncode(stringValue)

  def toJava:java.net.URI = {
//    def js = scheme.stringValue
//    val jui = authority.flatMap(_.userInfo).map(_.stringValue).orNull
//    val jh = authority.map(_.host.asciiStringValue).orNull
//    val jp = authority.flatMap(_.port).map(_.intValue).getOrElse(-1)
//    val jpath = path.map(_.stringValue).orNull
//    val jq = query.map(_.stringValue).orNull
//    val jf = fragment.map(_.stringValue).orNull
//    path match {
//      case Some(p) if !p.isAbsolute && !p.stringValue.isEmpty =>
//        val schemeSpecificPart = path.map(_.stringValue).getOrElse("") + query.map(q => "?" + q.stringValue).getOrElse("")
//        new java.net.URI(js, schemeSpecificPart, jf)
//      case _ => new java.net.URI(js, jui, jh, jp, jpath, jq, jf)
//    }
    new java.net.URI(stringValue)
  }
}

trait PathOnlyUri[PathType <: Path] extends Uri {

  type Self <: PathOnlyUri[PathType]

  def scheme: Scheme

  override def path: Option[PathType]

  def /(segment: Segment): Self

  def /(segment: String): Self = this / Segment(segment)

  def ?(query: Query): UriWithQuery[PathType] = UriWithQuery(scheme, authority, path, query)

  def ?(query: String): UriWithQuery[PathType] = this ? Query(query)

  def ##(fragment: String): CompleteUri[PathType] = this ## Fragment(fragment)

  def ##(f: Fragment): CompleteUri[PathType] = CompleteUri(scheme, authority, path, None, Some(f))

  override def query: None.type = None

  override def fragment: None.type = None

}

case class PathOnlyAuthorityUri(scheme: Scheme, definedAuthority: Authority, path: Option[AbsolutePath] = None) extends PathOnlyUri[AbsolutePath] {

  type Self = PathOnlyAuthorityUri

  override def authority: Some[Authority] = Some(definedAuthority)

  override def /(segment: Segment): PathOnlyAuthorityUri = {
    this.copy(path = Some(path.fold[AbsolutePath](Path / segment)(_ / segment)))
  }
}

case class PathOnlyAuthoritylessUri(scheme: Scheme, path: Option[Path] = None) extends PathOnlyUri[Path] {

  override type Self = PathOnlyAuthoritylessUri

  override def authority: None.type = None

  override def /(segment: Segment): PathOnlyAuthoritylessUri = {
    val newPath: Path = path.fold[Path](Path / segment)(_ / segment)
    this.copy(path = Some(newPath))
  }
}

case class UriWithQuery[PathType <: Path](scheme: Scheme, authority: Option[Authority], path: Option[PathType], definedQuery: Query) extends Uri {
  require(authority.nonEmpty || !path.exists(_.stringValue.startsWith("//")), s"An uri with no authority can't have a path starting with //: $this")

  override def query: Some[Query] = Some(definedQuery)

  override def fragment: None.type = None

  def ##(fragment: String): CompleteUri[PathType] = this ## Fragment(fragment)

  def ##(f: Fragment): CompleteUri[PathType] = CompleteUri(scheme, authority, path, Some(definedQuery), Some(f))

}

case class CompleteUri[PathType <: Path](scheme: Scheme, authority: Option[Authority], path: Option[PathType], query: Option[Query], fragment: Option[Fragment]) extends Uri {
  require(authority.nonEmpty || !path.exists(_.stringValue.startsWith("//")), s"An uri with no authority can't have a path starting with //: $this")
  require(authority.isDefined || path.isDefined || query.isDefined, s"sExpected scheme-specific part with scheme ${scheme.stringValue}")
}

object Uri {

  def apply(scheme: Scheme): PathOnlyAuthoritylessUri = PathOnlyAuthoritylessUri(scheme, None)

  def apply(scheme: String): PathOnlyAuthoritylessUri = Uri(Scheme(scheme))

  def apply(scheme: Scheme, path:Path): PathOnlyAuthoritylessUri = PathOnlyAuthoritylessUri(scheme, Some(path))

  def apply(scheme: String, path:Path): PathOnlyAuthoritylessUri = PathOnlyAuthoritylessUri(Scheme(scheme), Some(path))

  def apply(scheme: Scheme, host: RegisteredName): PathOnlyAuthorityUri = PathOnlyAuthorityUri(scheme, Authority(host))

  def apply(scheme: String, host: String): PathOnlyAuthorityUri = Uri(Scheme(scheme), RegisteredName(host))

  def apply(scheme: Scheme, userInfo: UserInfo, host: RegisteredName): PathOnlyAuthorityUri = PathOnlyAuthorityUri(scheme, Authority(userInfo, host))

  def apply(scheme: String, userInfo: String, host: String): PathOnlyAuthorityUri = Uri(Scheme(scheme), UserInfo(userInfo), RegisteredName(host))

  def apply(scheme: Scheme, host: RegisteredName, port: Port): PathOnlyAuthorityUri = PathOnlyAuthorityUri(scheme, Authority(host, port))

  def apply(scheme: String, host: String, port: Int): PathOnlyAuthorityUri = Uri(Scheme(scheme), RegisteredName(host), Port(port))

  def apply(scheme: Scheme, userInfo: UserInfo, host: RegisteredName, port: Port): PathOnlyAuthorityUri = PathOnlyAuthorityUri(scheme, Authority(userInfo, host, port))

  def apply(scheme: String, userInfo: String, host: String, port: Int): PathOnlyAuthorityUri = Uri(Scheme(scheme), UserInfo(userInfo), RegisteredName(host), Port(port))

  private val UriRe = "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?".r

  def parse(s:String):Try[Uri] = {
    def swap[T](optTry: Option[Try[T]]): Try[Option[T]] = {
      optTry.map(_.map(Some.apply)).getOrElse(Success(None))
    }

    val UriRe = "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?".r
    s match {
      case UriRe(_,sScheme,_,sAuthority,sPath,_,sQuery,_,sFragment) =>
        for {
          scheme <- Success(Option(sScheme).map(s => Scheme(Encoder.decode(s))))
          authority <- swap(Option(sAuthority).map(Authority.parse))
          path <- swap(Option(sPath).map(Path.parse)).map(_.flatten)
          query <- Success(Option(sQuery).map(q => Query(Encoder.decode(q))))
          fragment <- Success(Option(sFragment).map(f => Fragment(Encoder.decode(f))))
        } yield CompleteUri(scheme.get,authority,path,query,fragment)
      case _ =>
        Failure(new IllegalArgumentException())
    }

  }

}