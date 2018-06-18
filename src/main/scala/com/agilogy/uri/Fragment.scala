package com.agilogy.uri

abstract case class Fragment(stringValue: String) extends UriPart

object Fragment{
  def apply(stringValue:String): Fragment = new Fragment(Encoder.normalize(stringValue)){}
}
