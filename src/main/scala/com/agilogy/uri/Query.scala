package com.agilogy.uri

case class Query(stringValue: String) {
  def asciiStringValue: String = Encoder.asciiEncode(stringValue)
  override def toString: String = s"""Query("$stringValue")"""
}
