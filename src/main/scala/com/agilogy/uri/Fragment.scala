package com.agilogy.uri

case class Fragment(stringValue:String){
  def asciiStringValue:String = Encoder.asciiEncode(stringValue)
}
