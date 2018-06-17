package validation

import scala.language.implicitConversions
import scala.util.{ Failure, Success, Try }

trait ValidationModule {

  type ValidationError

  def fromThrowable(th: Throwable): ValidationError

  def fromTry[R](t: Try[R]): Validation[ValidationError, R] = t match {
    case Success(v)  => success(v)
    case Failure(th) => failure(fromThrowable(th))
  }

  case class MultipleErrorsException[E <: ValidationError](errors: List[E]) extends Exception{
    override def toString: String = {
      s"MultipleErrorsException(List(${errors.mkString(",")}))"
    }
  }

  type Validation[+E <: ValidationError, +R] = Either[List[E], R]
  type Valid[+R] = Right[Nothing, R]
  type Invalid[+E <: ValidationError] = Left[List[E], Nothing]

  def success[T](v: T): Valid[T] = Right(v)
  def failure[E <: ValidationError](e: E*): Invalid[E] = Left(e.toList)

  implicit def validationFunctor[E <: ValidationError, R1, R2](f: Either[List[E], R1 => R2]): ValidationFunctor[E, R1, R2] = new ValidationFunctor(f)

  class ValidationFunctor[E <: ValidationError, R1, R2](self: Validation[E, R1 => R2]) {

    def <*>[EE >: E <: ValidationError](other: Validation[EE, R1]): Validation[EE, R2] = (self, other) match {
      case (Left(e1), Left(e2)) => Left(e1 ++ e2)
      case (Left(e1), Right(_)) => Left(e1)
      case (Right(_), Left(e2)) => Left(e2)
      case (Right(f), Right(r)) => Right(f(r))
    }
  }

  def lift[R1, R2](f: R1 => R2): ValidationFunctor[Nothing, R1, R2] = new ValidationFunctor(success(f))

  implicit class ValidationOps[E <: ValidationError, R](v: Validation[E, R]) {

    def map[R2](f: R => R2): Validation[E, R2] = v.right.map(f)
    def flatMap[EE >: E <: ValidationError, R2](f: R => Validation[EE, R2]): Validation[EE, R2] = v.right.flatMap(f)
    //
    def toOption: Option[R] = v.fold(_ => None, Some.apply)
    def toTry: Try[R] = v.fold(l => Failure(MultipleErrorsException(l)), Success.apply)
    //
  }

  //  def validateExceptions[R](f: => R) = Try(f) match{
  //    case Success(v) => success(v)
  //    case Failure(t) => failure(t)
  //  }

  def swap[E <: ValidationError, R](optV: Option[Validation[E, R]]): Validation[E, Option[R]] = {
    optV.map(_.map(Some.apply)).getOrElse(success(None))
  }

}

object ValidationExceptions extends ValidationModule {
  override type ValidationError = Throwable

  override def fromThrowable(th: Throwable): Throwable = th

  //TODO: Remove when possible
  // This is a hack so that IntelliJ doesn't fail to recognize implicit methods in Validation
  implicit class ValidationExceptionOps[E <: ValidationError, R](v: Validation[E, R]) {
    val ops = ValidationOps(v)
    def map[R2](f: R => R2): Validation[E, R2] = ops.map(f)
    def flatMap[EE >: E <: ValidationError, R2](f: R => Validation[EE, R2]): Validation[EE, R2] = ops.flatMap(f)
    def toOption: Option[R] = ops.toOption
    def toTry: Try[R] = ops.toTry
    def validationToTry: Try[R] = ops.toTry

  }
}
