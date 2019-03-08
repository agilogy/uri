package com.agilogy.uri

import org.scalatest.{ EitherValues, FreeSpec }

class UriStringValueSpec extends FreeSpec with EitherValues {

  """
    |Uri stringValue""".stripMargin - {

    val http = Scheme("http").right.value
    val exampleDotCom = "example.com"
    val port = Port(8080).right.value
    val employees = "employees"
    val b12 = "b12"

    """Ascii uri string value""" in {
      //      val httpUri = RichUri(http)
      //      assert(httpUri.stringValue === "http:")
      val exampleComUri = RichUri(http, exampleDotCom)
      assert(exampleComUri.stringValue === "http://example.com")
      //      val johnDoeExampleComUri = RichUri(http, johnDoe, exampleDotCom)
      //      assert(johnDoeExampleComUri.stringValue === "http://johnDoe@example.com")
      val exampleComPortUri = RichUri(http, exampleDotCom, port)
      assert(exampleComPortUri.stringValue === "http://example.com:8080")
      //      val johnDoeExampleComPortUri = RichUri(http, johnDoe, exampleDotCom, port)
      //      assert(johnDoeExampleComPortUri.stringValue === "http://johnDoe@example.com:8080")
      val authorityPathUri = RichUri(http, exampleDotCom) / employees / b12
      assert(authorityPathUri.stringValue === "http://example.com/employees/b12")
      val authorityPortPathUri = RichUri(http, exampleDotCom, port) / employees / b12
      assert(authorityPortPathUri.stringValue === "http://example.com:8080/employees/b12")
      //      val fullUri = (RichUri(http, johnDoe, exampleDotCom, port) / employees / b12) ? withSalaryInfo ## salaryInfo
      //      assert(fullUri.stringValue === s"http://johnDoe@example.com:8080/employees/b12?withSalaryInfo#salaryInfo")
    }

    //    """Escape ':' in scheme """ in {
    //      assert((Uri("foo:bar", exampleDotCom)  / "a").stringValue === "foo%3Abar://example.com/a")
    //    }

    """Escape '/', '?' and '#' in authority""" in {
      assert(RichUri(http, Authority("john/jane?doe#", "example?/.foo#.com")).stringValue === "http://john%2Fjane%3Fdoe%23@example%3F%2F.foo%23.com")
    }

    """Escape '@' in userinfo""" in {
      assert(RichUri(http, Authority("j@ne", "example.com")).stringValue === "http://j%40ne@example.com")
    }

    """Escape ':' in registered name""" in {
      assert(RichUri(http, "ex:ample.com").stringValue === "http://ex%3Aample.com")
    }

    """Escape '?' and '#' in paths""" in {
      assert((RichUri(http, exampleDotCom) / "employee?" / "a#b").stringValue === "http://example.com/employee%3F/a%23b")
    }

    """Escape '#' in queries""" in {
      assert((RichUri(http, exampleDotCom) ? "query#").stringValue === "http://example.com?query%23")
    }

    """
      |The reg-name syntax allows percent-encoded octets in order to represent non-ASCII registered names in a uniform
      |way that is independent of the underlying name resolution technology.  Non-ASCII characters must first be
      |encoded according to UTF-8 [STD63], and then each octet of the corresponding UTF-8 sequence must be percent-
      |encoded to be represented as URI characters.  URI producing applications must not use percent-encoding in host
      |unless it is used to represent a UTF-8 character sequence.  When a non-ASCII registered name represents an
      |internationalized domain name intended for resolution via the DNS, the name must be transformed to the IDNA
      |encoding [RFC3490] prior to name lookup.  URI producers should provide these registered names in the IDNA
      |encoding, rather than a percent-encoding, if they wish to maximize interoperability with legacy URI resolvers.
    """.stripMargin in {
      //TODO: Implement
    }

    """
      |If a URI contains an authority component, then the path component must either be empty or begin with a slash
      |("/") character.  If a URI does not contain an authority component, then the path cannot begin with two slash
      |characters ("//").  In addition, a URI reference (Section 4.1) may be a relative-path reference, in which case
      |the first path segment cannot contain a colon (":") character.""".stripMargin in {
      //TODO: Implement
    }

  }

}
