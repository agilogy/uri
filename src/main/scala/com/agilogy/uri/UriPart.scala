package com.agilogy.uri

trait UriPart {

  def stringValue: String
  def asciiStringValue: String = Encoder.asciiEncode(stringValue)

}
