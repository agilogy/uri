package com.agilogy.uri

abstract case class Scheme private (stringValue: String) extends UriPart{
  override def asciiStringValue: String = stringValue
  override def toString: String = s"""Scheme("$stringValue")"""
  //  def ://(a:Authority)
}

object Scheme {

  def apply(v: String): Either[IllegalSchemeName,Scheme] = {
    if (!Encoder.isValidScheme(v)) Left(IllegalSchemeName(v))
    else Right(new Scheme(v.toLowerCase) {})
  }

}
