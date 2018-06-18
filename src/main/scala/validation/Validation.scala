package validation

import com.agilogy.uri._

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

trait ValidationModule {

  //TODO: Move validation implicits somewhere where they can be easily imported

  case class ValidationFailure[E](errors: List[E]) extends Exception {
    override def toString: String = {
      s"MultipleErrorsException(List(${errors.mkString(",")}))"
    }
  }

  type Validation[+E, +R] = Either[List[E], R]
  type Valid[+R] = Right[Nothing, R]
  type Invalid[+E] = Left[List[E], Nothing]

  def success[T](v: T): Valid[T] = Right(v)
  def failure[E](e: E*): Invalid[E] = Left(e.toList)

  implicit def validationFunctor[E, R1, R2](f: Either[List[E], R1 => R2]): ValidationFunctor[E, R1, R2] = new ValidationFunctor(f)

  class ValidationFunctor[E, R1, R2](self: Validation[E, R1 => R2]) {

    def <*>[EE >: E](other: Validation[EE, R1]): Validation[EE, R2] = (self, other) match {
      case (Left(e1), Left(e2)) => Left(e1 ++ e2)
      case (Left(e1), Right(_)) => Left(e1)
      case (Right(_), Left(e2)) => Left(e2)
      case (Right(f), Right(r)) => Right(f(r))
    }
  }

  def lift[R1, R2](f: R1 => R2): ValidationFunctor[Nothing, R1, R2] = new ValidationFunctor(success(f))

  implicit class ValidationOps[E, R](v: Validation[E, R]) {

    def map[R2](f: R => R2): Validation[E, R2] = v.right.map(f)
    def flatMap[EE >: E, R2](f: R => Validation[EE, R2]): Validation[EE, R2] = v.right.flatMap(f)
    def andValidate[EE >: E, R2](f: R => Either[EE, R2]): Validation[EE, R2] = v.right.flatMap{
      r => f(r).left.map(e => List(e))
    }
    //
    def toOption: Option[R] = v.fold(_ => None, Some.apply)
    def toTry: Try[R] = v.fold(l => Failure(ValidationFailure(l)), Success.apply)
    def validationToTry: Try[R] = toTry
    //
  }

  def swap[E, R](optV: Option[Validation[E, R]]): Validation[E, Option[R]] = {
    optV.map(_.map(Some.apply)).getOrElse(success(None))
  }

  def sequence[E,R](optE: Option[Either[E,R]]):Either[E, Option[R]] = {
    optE.map(_.map(Some.apply)).getOrElse(Right(None))
  }

  def toValidation[E,R](self:Either[E,R]):Validation[E,R] = {
    self.left.map(e => List(e))
  }

  implicit class PathVOps[E](self:Either[E,RootlessPath]){
    def /(s: Segment):Either[E, RootlessPath#PathWithSegmentsType] = self.right.map(_ / s)
    def /(s: String): Either[E, RootlessPath#PathWithSegmentsType] = this / Segment(s)
  }

  implicit class NoAuthorityPathUriVOps(self: Either[PathStartsWithDoubleSlashInNoAuhtorityUri,NoAuthorityPathUri]){
    def /(s: Segment): Either[PathError, NoAuthorityPathUri] = self.flatMap(_ / s)
    def /(s: String): Either[PathError, NoAuthorityPathUri] = this / Segment(s)

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

}

object Validation extends ValidationModule
