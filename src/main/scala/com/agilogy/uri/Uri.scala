package com.agilogy.uri

import scala.util.Try

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

trait NoQueryFragmentUri[U <: Uri] extends NoQueryUri with NoFragmentUri[U]{
  type UQ <: U with QueryUri with NoFragmentUri[U]

  def ?(query: Query): UQ
  def ?(query: String): UQ = this ? Query(query)
  def q(query: String): UQ = this ? Query(query)

}

trait FragmentUri extends Uri {
  def theFragment: Fragment

  override def fragment: Some[Fragment] = Some(theFragment)
}

case class NoAuthorityPathUri(scheme: Scheme, path: Path) extends NoAuthorityUri with NoQueryFragmentUri[NoAuthorityUri] {

  override type UQ = NoAuthorityPathQUri
  override type UF = NoAuthorityPathFUri

  // See https://tools.ietf.org/html/rfc3986#section-3.3
  // If a URI does not contain an authority component, then the path cannot begin with two slash characters ("//").
  require(!path.stringValue.startsWith("//"), s"An uri with no authority can't have a path starting with //: $this")
//  require(!path.isEmpty || query.isDefined, s"sExpected scheme-specific part with scheme ${scheme.stringValue}")
  override def ?(q: Query): NoAuthorityPathQUri = NoAuthorityPathQUri(scheme, path, q)

  override def ##(f: Fragment): NoAuthorityPathFUri = NoAuthorityPathFUri(scheme, path, f)
}

case class NoAuthorityPathQUri(scheme: Scheme, path: Path, theQuery:Query) extends NoAuthorityUri with QueryUri with NoFragmentUri[NoAuthorityUri] {
  override type UF = NoAuthorityPathQFUri
  override def ##(f: Fragment): NoAuthorityPathQFUri = NoAuthorityPathQFUri(scheme, path, theQuery, f)
}
case class NoAuthorityPathFUri(scheme: Scheme, path: Path, theFragment: Fragment) extends NoAuthorityUri with NoQueryUri with FragmentUri
case class NoAuthorityPathQFUri(scheme: Scheme, path: Path, theQuery:Query, theFragment: Fragment) extends NoAuthorityUri with QueryUri with FragmentUri

case class AuthorityPathUri(scheme: Scheme, theAuthority: Authority, path: PathAbEmpty = Path.empty) extends AuthorityUri with NoQueryFragmentUri[AuthorityUri] {
  self =>

  override type UQ = AuthorityPathQUri
  override type UF = AuthorityPathFUri

  def /(s: Segment): AuthorityPathUri = this.copy(path = path / s)
  def /(s: String): AuthorityPathUri = this / Segment(s)

  override def ?(q: Query): AuthorityPathQUri = AuthorityPathQUri(scheme, theAuthority, path, q)

  override def ##(f: Fragment): AuthorityPathFUri = AuthorityPathFUri(scheme, theAuthority, path, f)
}

case class AuthorityPathQUri(scheme: Scheme, theAuthority: Authority, path: PathAbEmpty = Path.empty, theQuery:Query) extends AuthorityUri with QueryUri with NoFragmentUri[AuthorityUri] {
  override type UF = AuthorityPathQFUri
  override def ##(f: Fragment): AuthorityPathQFUri = AuthorityPathQFUri(scheme, theAuthority, path, theQuery, f)
}

case class AuthorityPathFUri(scheme: Scheme, theAuthority: Authority, path: PathAbEmpty = Path.empty, theFragment: Fragment) extends AuthorityUri with NoQueryUri with FragmentUri

case class AuthorityPathQFUri(scheme: Scheme, theAuthority: Authority, path: PathAbEmpty = Path.empty, theQuery:Query, theFragment: Fragment) extends AuthorityUri with QueryUri with FragmentUri

object Uri {

  def apply(s: Scheme, p: Path): NoAuthorityPathUri = NoAuthorityPathUri(s, p)
  def apply(scheme: String, p: Path): NoAuthorityPathUri = Uri(Scheme(scheme), p)

  def apply(s: Scheme, authority: Authority): AuthorityPathUri = AuthorityPathUri(s, authority, Path.empty)
  def apply(scheme: String, host: String): AuthorityPathUri = Uri(Scheme(scheme), Authority(Host(host)))
  def apply(scheme: String, host: String, port:Int): AuthorityPathUri = Uri(Scheme(scheme), Authority(host, port))

  def apply(scheme: Scheme, authority: Authority, path: PathAbEmpty = Path.empty, query: Option[Query] = None, fragment: Option[Fragment] = None):AuthorityUri = {
    (path, query, fragment) match{
      case (p:PathAbEmpty, None, None) => AuthorityPathUri(scheme, authority, p)
      case (p:PathAbEmpty, Some(q), None) => AuthorityPathQUri(scheme, authority, p, q)
      case (p:PathAbEmpty, None, Some(f)) => AuthorityPathFUri(scheme, authority, p, f)
      case (p:PathAbEmpty, Some(q), Some(f)) => AuthorityPathQFUri(scheme, authority, p, q, f)
    }
  }

  def noAuthority(scheme: Scheme, path: Path, query: Option[Query] = None, fragment: Option[Fragment] = None):NoAuthorityUri = {
    (path, query, fragment) match{
      case (_, None, None) => NoAuthorityPathUri(scheme, path)
      case (_, Some(q), None) =>  NoAuthorityPathQUri(scheme, path, q)
      case (_, None, Some(f)) => NoAuthorityPathFUri(scheme, path, f)
      case (_, Some(q), Some(f)) => NoAuthorityPathQFUri(scheme, path, q, f)
    }
  }

  def of(scheme: Scheme, authority: Option[Authority], path: Path, query: Option[Query] = None, fragment: Option[Fragment] = None):Uri = {
    (authority, path) match {
      case (Some(a), p:PathAbEmpty) => Uri(scheme, a, p, query, fragment)
      case (Some(_), _) => throw new IllegalArgumentException("Path can't be rootless")
      case (None, _) => Uri.noAuthority(scheme, path, query, fragment)
    }
  }
    // This is infact the regexp for URI-Reference, not for URI
  // See https://tools.ietf.org/html/rfc3986#appendix-B
  private val UriRe = "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?".r

  import validation.ValidationExceptions._

  def parseTry(s: String): Try[Uri] = parse(s).validationToTry

  def parse(s: String): Validation[Throwable, Uri] = {

    import validation.Validators._

    s match {
      case UriRe(_, sScheme, _, sAuthority, sPath, _, sQuery, _, sFragment) =>
        lift((Uri.of _).curried) <*>
          notNull("scheme", sScheme).map(Scheme.apply) <*>
          swap(Option(sAuthority).map(Authority.parse)) <*>
          notNull("path", sPath).flatMap(p => fromTry(Path.parse(p))) <*>
          success(Option(sQuery).map(q => Query(Encoder.decode(q)))) <*>
          success(Option(sFragment).map(f => Fragment(Encoder.decode(f))))
      case _ =>
        // It actually never happens. Every possible string matches de url regexp, afaik
        failure(new DoesNotMatch("uri", UriRe)) //Failure(new IllegalArgumentException())
    }

  }

}