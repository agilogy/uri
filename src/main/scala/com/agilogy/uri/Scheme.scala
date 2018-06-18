package com.agilogy.uri

import validation.Validation._

abstract case class Scheme private (stringValue: String) {
  def asciiStringValue: String = stringValue
  override def toString: String = s"""Scheme("$stringValue")"""
  //  def ://(a:Authority)
}

object Scheme {

  def apply(v: String): Either[IllegalSchemeName,Scheme] = {
    if (!Encoder.isValidScheme(v)) Left(IllegalSchemeName(v))
    else success(new Scheme(v.toLowerCase) {})
  }

}
