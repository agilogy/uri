package com.agilogy.uri

import org.scalatest.{ FlatSpec, PrivateMethodTester }

class EncoderSpec extends FlatSpec with PrivateMethodTester {

  //  val encode = PrivateMethod[String]('encode)
  //  val chars = PrivateMethod[Iterator[String]]('chars)

  behavior of "Encoder"

  //  it should "iterate over non encoded characters" in{
  //    val s = "abc"
  //    val i = Encoder invokePrivate chars(s)
  //    assert(i.hasNext)
  //    assert(i.next() === "a")
  //    assert(i.hasNext)
  //    assert(i.next() === "b")
  //    assert(i.hasNext)
  //    assert(i.next() === "c")
  //    assert(!i.hasNext)
  //  }

  //  it should "iterate over possibly encoded characters" in {
  //    val s = "a%20b"
  //    val i = Encoder invokePrivate chars(s)
  //    assert(i.hasNext)
  //    assert(i.next() === "a")
  //    assert(i.hasNext)
  //    assert(i.next() === "%20")
  //    assert(i.hasNext)
  //    assert(i.next() === "b")
  //    assert(!i.hasNext)
  //  }

  //  it should "encode and decode characters" in {
  //    val s = "colon:slash/questionMark?hash#squareO[squareC]add@"
  //    val encoded = Encoder invokePrivate encode(s, Set.empty)
  //    assert(encoded === "colon%3Aslash%2FquestionMark%3Fhash%23squareO%5BsquareC%5Dadd%40")
  //    assert(Encoder.decode(encoded) === s)
  //  }

  it should "decode a simple string" in {
    assert(Encoder.decode("hello") === "hello")
  }

//    it should "encode non ascii chars" in {
//      val s = "㮜"
//      val encoded = Encoder invokePrivate encode(s, Set.empty)
//      val res = Encoder.decode(encoded)
//      assert(res === s, s"$s is encoded as $encoded but not decoded to $s but to $res")
//    }

}
