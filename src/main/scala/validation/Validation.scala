package validation

import com.agilogy.uri.{NoAuthorityPathQUri, PathStartsWithDoubleSlashInNoAuhtorityUri, _}

trait Validation {

  def sequence[E, R](optE: Option[Either[E, R]]): Either[E, Option[R]] = {
    optE.map(_.map(Some.apply)).getOrElse(Right(None))
  }

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

  def notNull[E, T](error: E, v: Option[T]): Either[E, T] = v.map(Right.apply).getOrElse(Left(error))

}

object Validation extends Validation
