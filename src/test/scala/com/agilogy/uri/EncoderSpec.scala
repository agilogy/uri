package com.agilogy.uri

import org.scalatest.FlatSpec

class EncoderSpec extends FlatSpec{

  behavior of "Encoder"

  it should "iterate over non encoded characters" in{
    val s = "abc"
    val i = Encoder.chars(s)
    assert(i.hasNext)
    assert(i.next() === "a")
    assert(i.hasNext)
    assert(i.next() === "b")
    assert(i.hasNext)
    assert(i.next() === "c")
    assert(!i.hasNext)
  }

  it should "iterate over possibly encoded characters" in {
    val s = "a%20b"
    val i = Encoder.chars(s)
    assert(i.hasNext)
    assert(i.next() === "a")
    assert(i.hasNext)
    assert(i.next() === "%20")
    assert(i.hasNext)
    assert(i.next() === "b")
    assert(!i.hasNext)
  }

  it should "encode and decode characters" in {
    val s = "colon:slash/questionMark?hash#squareO[squareC]add@"
    val charactersToEncode = ":/?#[]@".toSet
    val encoded = Encoder.encode(s, charactersToEncode)
    assert(encoded === "colon%3Aslash%2FquestionMark%3Fhash%23squareO%5BsquareC%5Dadd%40")
    assert(Encoder.decode(encoded) === s)
  }

  it should "decode a simple string" in {
    assert(Encoder.decode("hello") === "hello")
  }

}
