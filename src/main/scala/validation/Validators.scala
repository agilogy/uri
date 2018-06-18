package validation

import Validation._
import Validation.Validation

object Validators {

  def notNull[E, T](error: E, v: T): Validation[E, T] = if (v == null) failure(error) else success(v)

}
