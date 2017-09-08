package com.agilogy.uri

abstract case class Scheme private (stringValue: String) {
  def asciiStringValue: String = stringValue
  override def toString: String = s"""Scheme("$stringValue")"""
  //  def ://(a:Authority)
}

object Scheme {

  def IllegalSchemeName(s: String) =
    s"""Illegal scheme $s
      |Scheme names consist of a sequence of characters beginning with a letter and followed by any combination of
      |letters, digits, plus ("+"), period ("."), or hyphen ("-").""".stripMargin

  def apply(v: String): Scheme = {
    require(Encoder.isValidScheme(v), IllegalSchemeName(v))
    new Scheme(v.toLowerCase) {}
  }

}
