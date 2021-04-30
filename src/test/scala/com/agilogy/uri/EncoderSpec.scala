package com.agilogy.uri

import org.scalatest.{ FlatSpec, PrivateMethodTester }

class EncoderSpec extends FlatSpec with PrivateMethodTester {

  behavior of "Encoder"

  it should "decode a simple string" in {
    assert(Encoder.decode("hello") === "hello")
  }

}
