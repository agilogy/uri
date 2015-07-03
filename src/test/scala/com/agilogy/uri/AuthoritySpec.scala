package com.agilogy.uri

import org.scalatest.FlatSpec

class AuthoritySpec extends FlatSpec{

  behavior of "Authority"

  it should "build an authority with host only" in {
    val a = Authority("example.com")
    assert(a.userInfo.isEmpty)
    assert(a.host.value === "example.com")
    assert(a.port.isEmpty)
    assert(a.toString === "example.com")
  }

  it should "build an authority with host and port" in {
    val a = Authority("example.com",80)
    assert(a.userInfo.isEmpty)
    assert(a.host.value === "example.com")
    assert(a.port.isDefined)
    assert(a.port.get.value === 80)
    assert(a.toString === "example.com:80")
  }

  it should "build an authority with all three parts" in {
    val a = Authority("jordi","example.com",80)
    assert(a.userInfo.isDefined)
    assert(a.userInfo.get.value === "jordi")
    assert(a.host.value === "example.com")
    assert(a.port.isDefined)
    assert(a.port.get.value === 80)
    assert(a.toString === "jordi@example.com:80")
  }

}
