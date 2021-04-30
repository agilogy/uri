package com.agilogy.uri

sealed trait RichUri extends Uri {
  override def stringValue: String = Encoder.quoteUri(this)
}

object RichUri {
  def apply(s: Scheme): NoAuthorityPathUri = NoAuthorityPathUri(s)

  def apply(s: Scheme, p: Path): Either[PathStartsWithDoubleSlashInNoAuhtorityUri, NoAuthorityPathUri] = NoAuthorityPathUri(s, p)

  def apply(s: Scheme, authority: Authority): AuthorityPathUri = AuthorityPathUri(s, authority, Path.empty)

  def apply(scheme: Scheme, host: String): AuthorityPathUri = RichUri(scheme, Authority(Host(host)))

  def apply(scheme: Scheme, host: String, port: Port): AuthorityPathUri = RichUri(scheme, Authority(Host(host), port))

  def apply(scheme: Scheme, host: String, port: Int): Either[NegativePort, AuthorityPathUri] = Port(port).map(p => RichUri(scheme, Authority(Host(host), p)))

  def apply(scheme: Scheme, authority: Authority, path: PathAbEmpty = Path.empty, query: Option[Query] = None, fragment: Option[Fragment] = None): AuthorityUri = {
    (path, query, fragment) match {
      case (p: PathAbEmpty, None, None)       => AuthorityPathUri(scheme, authority, p)
      case (p: PathAbEmpty, Some(q), None)    => AuthorityPathQUri(scheme, authority, p, q)
      case (p: PathAbEmpty, None, Some(f))    => AuthorityPathFUri(scheme, authority, p, f)
      case (p: PathAbEmpty, Some(q), Some(f)) => AuthorityPathQFUri(scheme, authority, p, q, f)
    }
  }

  def noAuthority(scheme: Scheme, segment: String): NoAuthorityPathUri = {
    Path(segment) match {
      case Path.empty      => NoAuthorityPathUri(scheme)
      case p: RootlessPath => NoAuthorityPathUri(scheme, p)
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

}

trait NoAuthorityUri extends RichUri {
  override val authority: None.type = None
}

trait AuthorityUri extends RichUri {
  def theAuthority: Authority

  override def authority: Some[Authority] = Some(theAuthority)
}

trait NoQueryUri extends RichUri {
  override def query: None.type = None
}

trait QueryUri extends RichUri {
  def theQuery: Query

  override def query: Some[Query] = Some(theQuery)
}

trait NoFragmentUri[U <: RichUri] extends RichUri {
  override def fragment: None.type = None

  type UF <: U with FragmentUri

  def ##(f: Fragment): UF

  def ##(f: String): UF = this ## Fragment(f)

  def f(f: Fragment): UF = this ## f

  def f(f: String): UF = this ## Fragment(f)
}

trait NoQueryFragmentUri[U <: RichUri] extends NoQueryUri with NoFragmentUri[U] {
  type UQ <: U with QueryUri with NoFragmentUri[U]

  def ?(query: Query): UQ

  def ?(query: String): UQ = this ? Query(query)

  def q(query: Query): UQ = this ? query

  def q(query: String): UQ = this ? Query(query)

}

trait FragmentUri extends RichUri {
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

object NoAuthorityPathUri {
  def apply(scheme: Scheme): NoAuthorityPathUri = new NoAuthorityPathUri(scheme, Path.empty) {}

  def apply(scheme: Scheme, path: RootlessPath): NoAuthorityPathUri = new NoAuthorityPathUri(scheme, path) {}

  def apply(scheme: Scheme, path: Path): Either[PathStartsWithDoubleSlashInNoAuhtorityUri, NoAuthorityPathUri] = {

    // See https://tools.ietf.org/html/rfc3986#section-3.3
    // If a URI does not contain an authority component, then the path cannot begin with two slash characters ("//").
    if (path.stringValue.startsWith("//")) {
      Left(PathStartsWithDoubleSlashInNoAuhtorityUri(scheme, path))
    } else {
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

