package com.agilogy.uri

sealed abstract case class Scheme private(stringValue: String) extends UriPart {
  override def asciiStringValue: String = stringValue
}

object Scheme {

  def apply(v: String): Either[IllegalSchemeName, Scheme] = {
    if (!Encoder.isValidScheme(v)) Left(IllegalSchemeName(v))
    else Right(unsafe(v))
  }

  private[uri] def unsafe(v: String): Scheme = new Scheme(v.toLowerCase) {}

}
