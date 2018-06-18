package com.agilogy.uri

abstract case class Query(stringValue: String) extends UriPart {
  override def toString: String = s"""Query("$stringValue")"""
}

object Query{
  def apply(stringValue:String): Query = new Query(Encoder.normalize(stringValue)){}
}
