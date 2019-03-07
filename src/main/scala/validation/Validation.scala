package validation

import com.agilogy.uri.{ NoAuthorityPathQUri, PathStartsWithDoubleSlashInNoAuhtorityUri, _ }

trait Validation {

  //TODO: Move validation implicits somewhere where they can be easily imported

  def sequence[E, R](optE: Option[Either[E, R]]): Either[E, Option[R]] = {
    optE.map(_.map(Some.apply)).getOrElse(Right(None))
  }

  //  implicit class PathVOps[E](self: Either[E, RootlessPath]) {
  //    def /(s: Segment): Either[E, RootlessPath#PathWithSegmentsType] = self.right.map(_ / s)
  //    def /(s: String): Either[E, RootlessPath#PathWithSegmentsType] = this / Segment(s)
  //  }

  implicit class NoAuthorityPathUriVOps(self: Either[PathStartsWithDoubleSlashInNoAuhtorityUri, NoAuthorityPathUri]) {
    def /(s: Segment): Either[PathStartsWithDoubleSlashInNoAuhtorityUri, NoAuthorityPathUri] = self.flatMap(_ / s)
    def /(s: String): Either[PathStartsWithDoubleSlashInNoAuhtorityUri, NoAuthorityPathUri] = this / Segment(s)

    def ?(query: Query): Either[PathStartsWithDoubleSlashInNoAuhtorityUri, NoAuthorityPathQUri] = self.map(_ ? query)
    def ?(query: String): Either[PathStartsWithDoubleSlashInNoAuhtorityUri, NoAuthorityPathQUri] = self.map(_ ? query)
    def q(query: Query): Either[PathStartsWithDoubleSlashInNoAuhtorityUri, NoAuthorityPathQUri] = self.map(_ q query)
    def q(query: String): Either[PathStartsWithDoubleSlashInNoAuhtorityUri, NoAuthorityPathQUri] = self.map(_ q query)

    def ##(fragment: Fragment): Either[PathStartsWithDoubleSlashInNoAuhtorityUri, NoAuthorityPathFUri] = self.map(_ ## fragment)
    def ##(fragment: String): Either[PathStartsWithDoubleSlashInNoAuhtorityUri, NoAuthorityPathFUri] = self.map(_ ## fragment)
    def f(fragment: Fragment): Either[PathStartsWithDoubleSlashInNoAuhtorityUri, NoAuthorityPathFUri] = self.map(_ f fragment)
    def f(fragment: String): Either[PathStartsWithDoubleSlashInNoAuhtorityUri, NoAuthorityPathFUri] = self.map(_ f fragment)
  }

  implicit class NoAuthorityPathQUriVOps(self: Either[PathStartsWithDoubleSlashInNoAuhtorityUri, NoAuthorityPathQUri]) {
    def ##(fragment: Fragment): Either[PathStartsWithDoubleSlashInNoAuhtorityUri, NoAuthorityPathQFUri] = self.map(_ ## fragment)
    def ##(fragment: String): Either[PathStartsWithDoubleSlashInNoAuhtorityUri, NoAuthorityPathQFUri] = self.map(_ ## fragment)
    def f(fragment: Fragment): Either[PathStartsWithDoubleSlashInNoAuhtorityUri, NoAuthorityPathQFUri] = self.map(_ f fragment)
    def f(fragment: String): Either[PathStartsWithDoubleSlashInNoAuhtorityUri, NoAuthorityPathQFUri] = self.map(_ f fragment)

  }

  //  trait QueryOps[S<:NoQueryFragmentUri[_], R<:QueryUri]{
  //    def self: Either[IllegalSchemeName, S]
  //    def ?(query: Query): Either[IllegalSchemeName, R] = self.right.map(u => (u ? query).asInstanceOf[R])
  //    def ?(query: String): Either[IllegalSchemeName, R] = this ? Query(query)
  //    def q(query: String): Either[IllegalSchemeName, R] = this ? Query(query)
  //  }
  //
  //  trait FragmentOps[S<:NoFragmentUri[_],R<:FragmentUri]{
  //    def self: Either[IllegalSchemeName, S]
  //    def ##(f: Fragment): Either[IllegalSchemeName,R] = self.right.map(u => (u ## f).asInstanceOf[R])
  //    def ##(f: String): Either[IllegalSchemeName,R] = this ## Fragment(f)
  //    def f(f: String): Either[IllegalSchemeName, R] = this ## Fragment(f)
  //
  //  }
  //
  //  implicit class AuthorityPathUriVOps(val self: Either[IllegalSchemeName, AuthorityPathUri])
  //    extends FragmentOps[AuthorityPathUri, AuthorityPathFUri]
  //      with QueryOps[AuthorityPathUri, AuthorityPathQUri] {
  //    def /(s: Segment): Either[IllegalSchemeName, AuthorityPathUri] = self.right.map(_ / s)
  //    def /(s: String): Either[IllegalSchemeName, AuthorityPathUri] = this / Segment(s)
  //  }
  //
  //  implicit class AuthorityPathQUriVOps(val self: Either[IllegalSchemeName, AuthorityPathQUri])
  //    extends FragmentOps[AuthorityPathQUri, AuthorityPathQFUri] {
  //  }

  def notNull[E, T](error: E, v: Option[T]): Either[E, T] = v.map(Right.apply).getOrElse(Left(error))

}

object Validation extends Validation
