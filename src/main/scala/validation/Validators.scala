package validation

import ValidationExceptions._
import ValidationExceptions.Validation

import scala.util.matching.Regex

object Validators {

  case class IsEmpty(context:String) extends Exception{
    override def toString: String = s"IsEmpty($context)"
  }
  def notEmpty[T <: AnyRef](context:String, v: Option[T]): Validation[IsEmpty, T] = v match {
    case None => failure(IsEmpty(context))
    case Some(vv) => success(vv)
  }

  case class IsNull(context:String) extends Exception{
    override def toString: String = s"IsNull($context)"
  }
  def notNull[T <: AnyRef](context:String, v: T): Validation[IsNull, T] = if (v == null) failure(new IsNull(context)) else success(v)

  case class DoesNotMatch(context:String, re: Regex) extends Exception

}
