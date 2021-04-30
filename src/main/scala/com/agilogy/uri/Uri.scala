package com.agilogy.uri

import scala.util.Try

//TODO: Implement other subclasses of UriReference
sealed trait UriReference extends Any

trait Uri extends Any with UriReference with UriPart {

  def scheme: Scheme

  def authority: Option[Authority]

  def path: Path

  def query: Option[Query]

  def fragment: Option[Fragment]

  def stringValue: String

  def toJava: java.net.URI = {
    new java.net.URI(stringValue)
  }

  override def equals(obj: Any): Boolean = obj match {
    case u: Uri => this.stringValue == u.stringValue
    case _      => false
  }
}

final case class PackedUri(stringValue: String) extends AnyVal with Uri {

  override def scheme: Scheme = Scheme.unsafe(UriParser.matchParts(stringValue)._1.get)

  override def authority: Option[Authority] = UriParser.matchParts(stringValue)._2.map(a => Authority.parse(a).right.get)

  override def path: Path = Path.parse(UriParser.matchParts(stringValue)._3.getOrElse(""))

  override def query: Option[Query] = UriParser.matchParts(stringValue)._4.map(q => Query(Encoder.decode(q)))

  override def fragment: Option[Fragment] = UriParser.matchParts(stringValue)._5.map(f => Fragment(Encoder.decode(f)))

  def asRich: RichUri = UriParser.parseUriRich(stringValue).right.get
}

object Uri {

  def of(scheme: Scheme, authority: Option[Authority], path: Path, query: Option[Query] = None, fragment: Option[Fragment] = None): Either[PathError, RichUri] = {
    (authority, path) match {
      case (Some(a), p: RootlessPath) => Left(RootlessPathInAuthorityUri(scheme, a, p))
      case (Some(a), p: PathAbEmpty)  => Right(RichUri(scheme, a, p, query, fragment))
      case (None, _)                  => RichUri.noAuthority(scheme, path, query, fragment)
    }
  }

  def parseTry(s: String): Try[PackedUri] = parse(s).left.map(UriParseException).toTry

  def parseTryRich(s: String): Try[RichUri] = parseRich(s).left.map(UriParseException).toTry

  def parse(value: String): Either[UriParseError, PackedUri] = UriParser.parseUriPacked(value)

  def parseRich(value: String): Either[UriParseError, RichUri] = UriParser.parseUriRich(value)
}