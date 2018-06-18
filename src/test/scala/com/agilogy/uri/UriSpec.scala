package com.agilogy.uri

import org.scalatest.{EitherValues, FreeSpec, Matchers, OptionValues}
import validation.Validation._

class UriSpec extends FreeSpec with OptionValues with Matchers with EitherValues{

  """
    |A Uniform Resource Identifier (URI) provides a simple and extensible means for identifying a resource.
    |The generic URI syntax consists of a hierarchical sequence of components referred to as the scheme, authority,
    |path, query, and fragment.
  """.stripMargin - {

    """URI       = scheme ":" hier-part [ "?" query ] [ "#" fragment ]
      |hier-part = "//" authority path-abempty
      |             / ... """.stripMargin - {

      val sHttp = "http"
      val http = Scheme(sHttp).right.value
      val sAless = "aless"
      val aless = Scheme(sAless).right.value

      //      val userInfo = "johnDoe"
      val host = "www.example.com"
      val iPort = 8080
      val port = Port(iPort).right.value
      //      val path = Path / "posts" / "23"

      "minimal authority uri" in {
        val minimalAuthorityUri = Uri(http, Authority(Host(host)))
        assert(minimalAuthorityUri.scheme === http)
        assert(minimalAuthorityUri.theAuthority === Authority(host))
        assert(minimalAuthorityUri.authority.value === Authority(host))
        assert(minimalAuthorityUri.path.isEmpty)
        assert(minimalAuthorityUri.query === None)
        assert(minimalAuthorityUri.fragment === None)
      }

      "minimal no authority uri" in {
        val minimalNoAuthorityUri = Uri(aless)
        assert(minimalNoAuthorityUri.scheme === aless)
        assert(minimalNoAuthorityUri.authority === None)
        assert(minimalNoAuthorityUri.path === Path.empty)
        assert(minimalNoAuthorityUri.query === None)
        assert(minimalNoAuthorityUri.fragment=== None)
      }

      "authority uri with host + port" in {
        val portInfoUri = Uri(http, Authority(Host(host), port))
        assert(portInfoUri.theAuthority.host.stringValue === host)
        assert(portInfoUri.theAuthority.port.value.intValue === iPort)
        assert(portInfoUri.authority.value === portInfoUri.theAuthority)
        assert(portInfoUri.path.isEmpty)
        assert(portInfoUri.query === None)
        assert(portInfoUri.fragment === None)
        //        assert(Uri(scheme, Authority(host, port)) === Uri(Scheme(scheme), RegisteredName(host), Port(port)))
      }

      "add a path to an authority uri" in {
        val uri = Uri(http, "www.example.com")
        val uri1 = uri / Segment("a")
        assert(uri1.path === Path / "a")
        assert(uri / "a" === uri1)
        val uri2 = uri1 / Segment("b")
        assert(uri2.path === Path / "a" / "b")
        assert(uri1 / "b" === uri2)
      }

      "add a path to a no authority uri" in {
        val uri = Uri(aless)
        val uri1: Either[PathStartsWithDoubleSlashInNoAuhtorityUri, NoAuthorityPathUri] = uri / Segment("a")
        assert(uri1.right.value.path === Path / "a")
        assert(uri / "a" === uri1)
        val uri2 = uri1 / Segment("b")
        assert(uri2.right.value.path === Path / "a" / "b")
        assert(uri1 / "b" === uri2)
      }

        "add a query to an authority uri" in {
        val uri = Uri(http, host)
        val uri1 = uri ? Query("name=john")
        assert(uri1.theQuery === Query("name=john"))
        assert(uri1.query.value === Query("name=john"))
        """uri1 / "a" """ shouldNot compile
        assert((uri ? "name=john") === uri1)
      }

      "add a fragment to an authority uri" in {
        val fragment = "address"
        val uri = Uri(http, host)
        val uri1 = uri ## Fragment(fragment)
        assert(uri1.theFragment === Fragment(fragment))
        assert(uri1.fragment.value === Fragment(fragment))
        """uri1 ## Fragment("foo")""" shouldNot compile
        """uri1 ? "a=b"""" shouldNot compile
        """uri1 / "foo"""" shouldNot compile
        assert((uri ## fragment) == uri1)
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
        val res = (Uri(http, "www.example.com") / "employees" / "23") ? "withSalaryInfo=true" ## "salaryInfo"
        assert(res.scheme === Scheme("http").right.value)
        assert(res.theAuthority === res.authority.value)
        assert(res.theAuthority.host === Host("www.example.com"))
        assert(res.path === Path / "employees" / "23")
        assert(res.theQuery === res.query.value)
        assert(res.query.value === Query("withSalaryInfo=true"))
        assert(res.fragment.value === Fragment("salaryInfo"))
        assert(res.theFragment === res.fragment.value)
      }

      "build an authority uri with an absolute path with no segments" in {
        val res1 = Uri(http, "www.example.com")
        assert(res1.path.isEmpty)
      }

      "build an authority uri ending in '/'" in {
        val res2 = Uri(http, "www.example.com") / ""
        assert(res2.path.isAbsolute)
        assert(res2.path.segments.size === 1)
        assert(res2.path.segments.head === Segment.Empty)
      }

      //      "build an uri without authority nor path" in {
      //        val minimalUri = Uri(Scheme("example"))
      //        assert(minimalUri.scheme === Scheme("example"))
      //        assert(minimalUri.path.isEmpty)
      //        assert(Uri("example") === minimalUri)
      //      }

      "build an uri without authority but with path" in {
        val mailto = Scheme("mailto").right.value
        val user = Path("john@example.com").right.value
        val authorityPathUri = Uri(mailto, user).right.value
        assert(authorityPathUri.scheme === mailto)
        assert(!authorityPathUri.path.isAbsolute)
        assert(authorityPathUri.path === user)
        val wrongPath = Path.Slash / "" / ""
        assert(Uri(mailto, wrongPath) === Left(PathStartsWithDoubleSlashInNoAuhtorityUri(mailto, wrongPath)))
      }



    }

  }

}
