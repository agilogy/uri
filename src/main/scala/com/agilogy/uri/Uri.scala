package com.agilogy.uri

import scala.util.Try

//TODO: Implement other subclasses of UriReference
sealed trait UriReference

trait Uri extends UriReference {

  def scheme: Scheme

  def authority: Option[Authority]

  def path: Path

  def query: Option[Query]

  def fragment: Option[Fragment]

  def stringValue: String = Encoder.quoteUri(this)

  def asciiStringValue: String = Encoder.asciiEncode(stringValue)

  def toJava: java.net.URI = {
    new java.net.URI(stringValue)
  }

  override def equals(obj: Any): Boolean = obj match {
    case u: Uri => this.stringValue == u.stringValue
    case _      => false
  }
}

trait NoAuthorityUri extends Uri {
  override val authority: None.type = None
}

trait AuthorityUri extends Uri {
  def theAuthority: Authority

  override def authority: Some[Authority] = Some(theAuthority)
}

trait NoQueryUri extends Uri {
  override def query: None.type = None
}

trait QueryUri extends Uri {
  def theQuery: Query
  override def query: Some[Query] = Some(theQuery)
}

trait NoFragmentUri[U <: Uri] extends Uri {
  override def fragment: None.type = None

  type UF <: U with FragmentUri

  def ##(f: Fragment): UF
  def ##(f: String): UF = this ## Fragment(f)
  def f(f: String): UF = this ## Fragment(f)
}

trait NoQueryFragmentUri[U <: Uri] extends NoQueryUri with NoFragmentUri[U] {
  type UQ <: U with QueryUri with NoFragmentUri[U]

  def ?(query: Query): UQ
  def ?(query: String): UQ = this ? Query(query)
  def q(query: String): UQ = this ? Query(query)

}

trait FragmentUri extends Uri {
  def theFragment: Fragment

  override def fragment: Some[Fragment] = Some(theFragment)
}

abstract case class NoAuthorityPathUri private (scheme: Scheme, path: Path) extends NoAuthorityUri with NoQueryFragmentUri[NoAuthorityUri] {

  override type UQ = NoAuthorityPathQUri
  override type UF = NoAuthorityPathFUri

  def /(s: Segment): Either[PathStartsWithDoubleSlashInNoAuhtorityUri, NoAuthorityPathUri] = NoAuthorityPathUri(scheme, path / s)
  def /(s: String): Either[PathStartsWithDoubleSlashInNoAuhtorityUri, NoAuthorityPathUri] = this / Segment(s)

  override def ?(q: Query): NoAuthorityPathQUri = NoAuthorityPathQUri(scheme, path, q)

  override def ##(f: Fragment): NoAuthorityPathFUri = NoAuthorityPathFUri(scheme, path, f)
}

object NoAuthorityPathUri{
  def apply(scheme: Scheme, path: EmptyPath.type) = new NoAuthorityPathUri(scheme, path){}
  def apply(scheme: Scheme, path: Path): Either[PathStartsWithDoubleSlashInNoAuhtorityUri, NoAuthorityPathUri] = {

    // See https://tools.ietf.org/html/rfc3986#section-3.3
    // If a URI does not contain an authority component, then the path cannot begin with two slash characters ("//").
    if (path.stringValue.startsWith("//")) {
      Left(PathStartsWithDoubleSlashInNoAuhtorityUri(scheme, path))
    }
    //else if(path.isEmpty && !query.isDefined) throw new RuntimeException(s"Expected scheme-specific part with scheme ${scheme.stringValue}")
    else {
      Right(new NoAuthorityPathUri(scheme, path) {})
    }

  }
}

case class NoAuthorityPathQUri(scheme: Scheme, path: Path, theQuery: Query) extends NoAuthorityUri with QueryUri with NoFragmentUri[NoAuthorityUri] {
  override type UF = NoAuthorityPathQFUri
  override def ##(f: Fragment): NoAuthorityPathQFUri = NoAuthorityPathQFUri(scheme, path, theQuery, f)
}
case class NoAuthorityPathFUri(scheme: Scheme, path: Path, theFragment: Fragment) extends NoAuthorityUri with NoQueryUri with FragmentUri
case class NoAuthorityPathQFUri(scheme: Scheme, path: Path, theQuery: Query, theFragment: Fragment) extends NoAuthorityUri with QueryUri with FragmentUri

case class AuthorityPathUri(scheme: Scheme, theAuthority: Authority, path: PathAbEmpty = Path.empty) extends AuthorityUri with NoQueryFragmentUri[AuthorityUri] {
  self =>

  override type UQ = AuthorityPathQUri
  override type UF = AuthorityPathFUri

  def /(s: Segment): AuthorityPathUri = this.copy(path = path / s)
  def /(s: String): AuthorityPathUri = this / Segment(s)

  override def ?(q: Query): AuthorityPathQUri = AuthorityPathQUri(scheme, theAuthority, path, q)

  override def ##(f: Fragment): AuthorityPathFUri = AuthorityPathFUri(scheme, theAuthority, path, f)
}

case class AuthorityPathQUri(scheme: Scheme, theAuthority: Authority, path: PathAbEmpty = Path.empty, theQuery: Query) extends AuthorityUri with QueryUri with NoFragmentUri[AuthorityUri] {
  override type UF = AuthorityPathQFUri
  override def ##(f: Fragment): AuthorityPathQFUri = AuthorityPathQFUri(scheme, theAuthority, path, theQuery, f)
}

case class AuthorityPathFUri(scheme: Scheme, theAuthority: Authority, path: PathAbEmpty = Path.empty, theFragment: Fragment) extends AuthorityUri with NoQueryUri with FragmentUri

case class AuthorityPathQFUri(scheme: Scheme, theAuthority: Authority, path: PathAbEmpty = Path.empty, theQuery: Query, theFragment: Fragment) extends AuthorityUri with QueryUri with FragmentUri

object Uri {

  def apply(s: Scheme): NoAuthorityPathUri = NoAuthorityPathUri(s, Path.empty)
  def apply(s: Scheme, p: Path): Either[PathStartsWithDoubleSlashInNoAuhtorityUri, NoAuthorityPathUri] = NoAuthorityPathUri(s, p)

  def apply(s: Scheme, authority: Authority): AuthorityPathUri = AuthorityPathUri(s, authority, Path.empty)
  def apply(scheme: Scheme, host: String): AuthorityPathUri = Uri(scheme, Authority(Host(host)))
  def apply(scheme: Scheme, host: String, port: Port): AuthorityPathUri = Uri(scheme, Authority(Host(host), port))
  def apply(scheme: Scheme, host: String, port: Int): Either[NegativePort, AuthorityPathUri] = Port(port).map(p => Uri(scheme, Authority(Host(host), p)))

  def apply(scheme: Scheme, authority: Authority, path: PathAbEmpty = Path.empty, query: Option[Query] = None, fragment: Option[Fragment] = None): AuthorityUri = {
    (path, query, fragment) match {
      case (p: PathAbEmpty, None, None)       => AuthorityPathUri(scheme, authority, p)
      case (p: PathAbEmpty, Some(q), None)    => AuthorityPathQUri(scheme, authority, p, q)
      case (p: PathAbEmpty, None, Some(f))    => AuthorityPathFUri(scheme, authority, p, f)
      case (p: PathAbEmpty, Some(q), Some(f)) => AuthorityPathQFUri(scheme, authority, p, q, f)
    }
  }

  def noAuthority(scheme: Scheme, path: Path, query: Option[Query] = None, fragment: Option[Fragment] = None): Either[PathStartsWithDoubleSlashInNoAuhtorityUri, NoAuthorityUri] = {
    (path, query, fragment) match {
      case (_, None, None)       => NoAuthorityPathUri(scheme, path)
      case (_, Some(q), None)    => Right(NoAuthorityPathQUri(scheme, path, q))
      case (_, None, Some(f))    => Right(NoAuthorityPathFUri(scheme, path, f))
      case (_, Some(q), Some(f)) => Right(NoAuthorityPathQFUri(scheme, path, q, f))
    }
  }

  def of(scheme: Scheme, authority: Option[Authority], path: Path, query: Option[Query] = None, fragment: Option[Fragment] = None): Either[PathError, Uri] = {
    (authority, path) match {
      case (Some(a), p: PathAbEmpty)  => Right(Uri(scheme, a, p, query, fragment))
      case (Some(a), p: RootlessPath) => Left(RootlessPathInAuthorityUri(scheme, a, p))
      case (None, _)                  => Uri.noAuthority(scheme, path, query, fragment)
    }
  }
  // This is infact the regexp for URI-Reference, not for URI
  // See https://tools.ietf.org/html/rfc3986#appendix-B
  private val UriRe = "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?".r

  import validation.Validation._

  def parseTry(s: String): Try[Uri] = parse(s).validationToTry

  def parse(s: String): Validation[UriParseError, Uri] = {

    import validation.Validators._

    s match {
      case UriRe(_, sScheme, _, sAuthority, sPath, _, sQuery, _, sFragment) =>
        val scheme: Validation[UriParseError, Scheme] = notNull(MissingScheme(s), sScheme).andValidate(Scheme.apply)
        val authority: Either[UriParseError, Option[Authority]] = sequence(Option(sAuthority).map(Authority.parse))
        val res = lift((Uri.of _).curried) <*>
          scheme <*>
          toValidation(authority) <*>
          success(Path.parse(sPath)) <*>
          success(Option(sQuery).map(q => Query(Encoder.decode(q)))) <*>
          success(Option(sFragment).map(f => Fragment(Encoder.decode(f))))
        // THe regexp guaranteees that if there are "//" then they are the authority
        res.right.map(_.right.get)
      case _ =>
        // It actually never happens. Every possible string matches de url regexp, afaik
        throw new IllegalStateException("Unreachable code")
    }

  }

}