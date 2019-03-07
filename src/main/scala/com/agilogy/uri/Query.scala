package com.agilogy.uri

sealed abstract case class Query(stringValue: String) extends UriPart

object Query {
  def apply(stringValue: String): Query = new Query(Encoder.normalize(stringValue)) {}
}
