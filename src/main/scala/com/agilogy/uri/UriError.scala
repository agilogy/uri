package com.agilogy.uri

trait UriParseError extends Exception

trait SchemeError extends UriParseError

case class MissingScheme(uri:String) extends SchemeError

case class IllegalSchemeName(scheme: String) extends SchemeError {
  val message: String =
    s"""Illegal scheme $scheme
       |Scheme names consist of a sequence of characters beginning with a letter and followed by any combination of
       |letters, digits, plus ("+"), period ("."), or hyphen ("-").""".stripMargin

  override def getMessage: String = message
}

case class AuthorityParseError(authority:String) extends UriParseError

case class NegativePort(port:Int)

trait PathError

case class RootlessPathInAuthorityUri(scheme: Scheme, authority: Authority, path: RootlessPath) extends PathError
case class PathStartsWithDoubleSlashInNoAuhtorityUri(scheme: Scheme, path: Path) extends PathError
case object FirstSegmentIsEmptyInRootlessPath
