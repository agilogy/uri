package com.agilogy.uri

// reg-name      = *( unreserved / pct-encoded / sub-delims )
abstract case class Host(stringValue: String) extends UriPart {
  override def toString: String = s"""Host("$stringValue")"""
}

object Host {

  def apply(v: String): Host = {
    new Host(Encoder.normalize(v).toLowerCase) {}
  }
}
