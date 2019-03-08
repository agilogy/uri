package com.agilogy.uri

trait UriPart extends Any{

  def stringValue: String
  def asciiStringValue: String = Encoder.asciiEncode(stringValue)

}
