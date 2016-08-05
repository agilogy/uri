package validation

import ValidationExceptions._
import ValidationExceptions.Validation

import scala.util.matching.Regex

object Validators {

  class IsEmpty extends Exception
  def nonEmpty[T](o:Option[T]):Validation[IsEmpty,T] = o.fold[Validation[IsEmpty,T]](failure(new IsEmpty))(success)

  class IsNull extends Exception
  def notNull[T<:AnyRef](v:T):Validation[IsNull,T] = if(v == null) failure(new IsNull) else success(v)

  case class DoesNotMatch(re:Regex) extends Exception

}
