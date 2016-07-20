package com.agilogy.uri

trait Host{
  def stringValue:String
  def asciiStringValue:String = Encoder.asciiEncode(stringValue)
}

abstract case class RegisteredName(stringValue:String) extends Host{
  override def toString: String = s"""RegisteredName("$stringValue")"""
}

object RegisteredName{

  def IllegalRegisteredName(registeredName: String) =
    s"""Illegal registered name: $registeredName""".stripMargin

  def apply(v:String):RegisteredName = {
//    require(Encoder.isValidRegisteredName(v), IllegalRegisteredName(v))
    new RegisteredName(Encoder.normalize(v).toLowerCase){}
  }
}
