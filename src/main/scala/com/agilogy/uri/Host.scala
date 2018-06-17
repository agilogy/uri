package com.agilogy.uri

// reg-name      = *( unreserved / pct-encoded / sub-delims )
abstract case class Host(stringValue: String)  {
  def asciiStringValue: String = Encoder.asciiEncode(stringValue)
  override def toString: String = s"""Host("$stringValue")"""
}

object Host {

//  def IllegalRegisteredName(registeredName: String) =
//    s"""Illegal registered name: $registeredName""".stripMargin

  def apply(v: String): Host = {
//        require(Encoder.isValidRegisteredName(v), IllegalRegisteredName(v))
    new Host(Encoder.normalize(v).toLowerCase) {}
  }
}
