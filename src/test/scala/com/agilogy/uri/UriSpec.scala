package com.agilogy.uri

import org.scalatest.FlatSpec

class UriSpec extends FlatSpec{

  behavior of "Uri"

  it should "build a simple absolute uri" in{
    val a = Uri.absolute("http",Authority("example.com"),Path.Slash / "departments")
    assert(a.scheme === Scheme.http)
    assert(a.authority.host === Host("example.com"))
    assert(a.path === Path.Slash / "departments")
    assert(a.query.isEmpty)
    assert(a.fragment.isEmpty)
    assert(a.toString === "http://example.com/departments")
  }

  it should "build an absolute uri with query" in{
    val q = "foo=bar&goo=har"
    val a = Uri.absolute("http",Authority("example.com"),Path.Slash / "departments", Some(q))
    assert(a.scheme === Scheme.http)
    assert(a.authority.host === Host("example.com"))
    assert(a.path === Path.Slash / "departments")
    assert(a.query.isDefined)
    assert(a.query.get.value === q)
    assert(a.fragment.isEmpty)
    assert(a.toString === s"http://example.com/departments?$q")
  }

  it should "build an absolute uri with query and fragment" in{
    val q = "foo=bar&goo=har"
    val f = "part1"
    val a = Uri.absolute("http",Authority("example.com"),Path.Slash / "departments", Some(q), Some(f))
    assert(a.scheme === Scheme.http)
    assert(a.authority.host === Host("example.com"))
    assert(a.path === Path.Slash / "departments")
    assert(a.query.isDefined)
    assert(a.query.get.value === q)
    assert(a.fragment.isDefined)
    assert(a.fragment.get.value === f)
    assert(a.toString === s"http://example.com/departments?$q#$f")
  }

}
