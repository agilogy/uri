package com.agilogy.uri

abstract case class UriParseError private(scheme: Option[SchemeError], authority: Option[AuthorityParseError])

object UriParseError {
  def apply(scheme: Option[SchemeError] = None, authority: Option[AuthorityParseError] = None): UriParseError = {
    require(scheme.isDefined || authority.isDefined)
    new UriParseError(scheme, authority) {}
  }
}

case class UriParseException(error: UriParseError) extends Exception

trait SchemeError

case class MissingScheme(uri: String) extends SchemeError

/**
  * Scheme names consist of a sequence of characters beginning with a letter and followed by any combination of letters,
  * digits, plus ("+"), period ("."), or hyphen ("-").
  *
  * @param scheme The illegal scheme
  */
case class IllegalSchemeName(scheme: String) extends SchemeError

case class AuthorityParseError(authority: String)

case class AuthorityParseException(error: AuthorityParseError) extends Exception

case class NegativePort(port: Int)

trait PathError

case class RootlessPathInAuthorityUri(scheme: Scheme, authority: Authority, path: RootlessPath) extends PathError

case class PathStartsWithDoubleSlashInNoAuhtorityUri(scheme: Scheme, path: Path) extends PathError

case object FirstSegmentIsEmptyInRootlessPath
