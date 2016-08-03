package com.agilogy.uri

import org.scalatest.{FreeSpec, OptionValues, ShouldMatchers}

class UriSpec extends FreeSpec with OptionValues with ShouldMatchers {

  """
    |A Uniform Resource Identifier (URI) provides a simple and extensible means for identifying a resource.
    |The generic URI syntax consists of a hierarchical sequence of components referred to as the scheme, authority,
    |path, query, and fragment.
  """.stripMargin - {

    """URI       = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
      |hier-part = "//" authority path-abempty
      |             / ... """.stripMargin - {

      val scheme = "http"
      val userInfo = "johnDoe"
      val host = "www.example.com"
      val port = 8080
      val path = Path / "posts" / "23"

      "minimal authority uri" in {
        val minimalAuthorityUri = Uri(Scheme(scheme), RegisteredName(host))
        assert(minimalAuthorityUri.scheme === Scheme(scheme))
        assert(minimalAuthorityUri.theAuthority === Authority(host))
        assert(minimalAuthorityUri.authority.value === Authority(host))
        assert(minimalAuthorityUri.path.isEmpty)
        assert(minimalAuthorityUri.query === None)
        assert(minimalAuthorityUri.fragment === None)
        assert(Uri(scheme, host) === Uri(Scheme(scheme), RegisteredName(host)))
      }

//      "authority uri with userInfo + host" in {
//        val userInfoUri = Uri(Scheme(scheme), UserInfo(userInfo), RegisteredName(host))
//        assert(userInfoUri.authority.value === Authority(userInfo, host))
//        assert(userInfoUri.path.isEmpty)
//        assert(userInfoUri.query === None)
//        assert(userInfoUri.fragment === None)
//        assert(Uri(scheme, userInfo, host) === Uri(Scheme(scheme), UserInfo(userInfo), RegisteredName(host)))
//      }

      "authority uri with host + port" in {
        val portInfoUri = Uri(Scheme(scheme), RegisteredName(host), Port(port))
        assert(portInfoUri.theAuthority === Authority(host, port))
        assert(portInfoUri.authority.value === Authority(host, port))
        assert(portInfoUri.path.isEmpty)
        assert(portInfoUri.query === None)
        assert(portInfoUri.fragment === None)
        assert(Uri(scheme, host, port) === Uri(Scheme(scheme), RegisteredName(host), Port(port)))
      }

//      "authority uri with userinfo + host + port" in {
//        val fullAuthorityUri = Uri(Scheme(scheme), UserInfo(userInfo), RegisteredName(host), Port(port))
//        assert(fullAuthorityUri.authority.value === Authority(userInfo, host, port))
//        assert(fullAuthorityUri.path.isEmpty)
//        assert(fullAuthorityUri.query === None)
//        assert(fullAuthorityUri.fragment === None)
//        assert(Uri(scheme, userInfo, host, port) === Uri(Scheme(scheme), UserInfo(userInfo), RegisteredName(host), Port(port)))
//      }

      "add a path to an authority uri" in {
        val uri = Uri("http", "www.example.com")
        val uri1 = uri / Segment("a")
        assert(uri1.path.value === Path / "a")
        assert(uri / "a" === uri1)
        val uri2 = uri1 / Segment("b")
        assert(uri2.path.value === Path / "a" / "b")
        assert(uri1 / "b" === uri2)
      }

      "add a query to an authority uri" in {
        val uri = Uri(scheme, host)
        val uri1 = uri ? Query("name=john")
        assert(uri1.theQuery === Query("name=john"))
        assert(uri1.query.value === Query("name=john"))
        """uri1 / "a" """ shouldNot compile
        assert((uri ? "name=john") === uri1)
      }

      "add a fragment to an authority uri" in {
        val fragment = "address"
        val uri = Uri(scheme,host)
        val uri1 = uri ## Fragment(fragment)
        assert(uri1.theFragment === Fragment(fragment))
        assert(uri1.fragment.value === Fragment(fragment))
        """uri1 ## Fragment("foo")""" shouldNot compile
        """uri1 ? "a=b"""" shouldNot compile
        """uri1 / "foo"""" shouldNot compile
        assert(uri ## fragment == uri1)
        val query = "name=john"
        val queryUri = uri ? query
        val uri2 = queryUri ## Fragment(fragment)
        assert(uri2.theQuery === Query(query))
        assert(uri2.theFragment === Fragment(fragment))
        """uri2 ## Fragment("foo")""" shouldNot compile
        """uri2 ? "a=b"""" shouldNot compile
        """uri2 / "foo"""" shouldNot compile
        assert((queryUri ## fragment) === uri2)
      }

      "build a full authority uri" in {
        val res = (Uri("http","www.example.com") / "employees" / "23") ? "withSalaryInfo=true" ## "salaryInfo"
        assert(res.scheme === Scheme("http"))
        assert(res.theAuthority === res.authority.value)
        assert(res.theAuthority.host === RegisteredName("www.example.com"))
        assert(res.path.value === Path / "employees" / "23")
        assert(res.theQuery === res.query.value)
        assert(res.query.value === Query("withSalaryInfo=true"))
        assert(res.fragment.value === Fragment("salaryInfo"))
        assert(res.theFragment === res.fragment.value)
      }

      "build an authority uri with an absolute path with no segments" in {
        val res1 = Uri("http", "www.example.com")
        assert(res1.path.isEmpty)
      }

      "build an authority uri ending in '/'" in {
        val res2 = Uri("http","www.example.com") / ""
        assert(res2.path.value.isAbsolute)
        assert(res2.path.value.segments.size === 1)
        assert(res2.path.value.segments.head === Segment.Empty)
      }

//      "build an uri without authority nor path" in {
//        val minimalUri = Uri(Scheme("example"))
//        assert(minimalUri.scheme === Scheme("example"))
//        assert(minimalUri.path.isEmpty)
//        assert(Uri("example") === minimalUri)
//      }

      "build an uri without authority but with path" in {
        val authorityPathUri = Uri(Scheme("mailto"),Path("john@example.com"))
        assert(authorityPathUri.scheme === Scheme("mailto"))
        assert(authorityPathUri.thePath === authorityPathUri.path.value)
        assert(!authorityPathUri.thePath.isAbsolute)
        assert(authorityPathUri.thePath.segments === Seq(Segment("john@example.com")))
        assert(Uri("mailto",Path("john@example.com")) === authorityPathUri)
      }

    }

  }

}
