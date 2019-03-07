package com.agilogy.uri

// reg-name      = *( unreserved / pct-encoded / sub-delims )
sealed abstract case class Host(stringValue: String) extends UriPart

object Host {

  def apply(v: String): Host = {
    new Host(Encoder.normalize(v).toLowerCase) {}
  }
}
