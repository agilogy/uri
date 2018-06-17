package com.agilogy.uri

import org.scalatest.FreeSpec

class AuthoritySpec extends FreeSpec {

  """
    |Many URI schemes include a hierarchical element for a naming authority so that governance of the name space defined
    |by the remainder of the URI is delegated to that authority (which may, in turn, delegate it further).  The generic
    |syntax provides a common means for distinguishing an authority based on a registered name or server address, along
    |with optional port and user information.""".stripMargin - {

    """The userinfo subcomponent may consist of a user name and, optionally, scheme-specific information about how to
      |gain authorization to access the resource.""".stripMargin - {

      "Userinfo subcomponents have a string representation" in {
        assert(UserInfo("johndoe").stringValue === "johndoe")
      }

      """Under normal circumstances, the only time when octets within a URI are percent-encoded is during the process of
        |producing the URI from its component parts.""".stripMargin in {
        assert(UserInfo(":/?#%").stringValue === ":/?#%")
      }

    }

    """The host subcomponent of authority is identified by an IP literal encapsulated within square brackets, an IPv4
      |address in dotted-decimal form, or a registered name.""".stripMargin - {

      """A host identified by a registered name is a sequence of characters usually intended for lookup within a locally
        |defined host or service name registry, though the URI's scheme-specific semantics may require that a specific
        |registry (or fixed name table) be used instead.  The most common name registry mechanism is the Domain Name
        |System (DNS).""".stripMargin - {

        "Registered names have a string representation" in {
          assert(Host("www.example.com").stringValue === "www.example.com")
        }

        """Although host is case-insensitive, producers and normalizers should use lowercase for registered names and
          |hexadecimal addresses for the sake ofnuniformity.""".stripMargin in {
          assert(Host("eXaMpLÈ.coM").stringValue === "examplè.com"
          )
        }

        """Under normal circumstances, the only time when octets within a URI are percent-encoded is during the process of
          |producing the URI from its component parts.""".stripMargin in {
          assert(Host(":/?#%").stringValue === ":/?#%")
        }

      }

    }

    """The port subcomponent of authority is designated by an optional port number in decimal""" - {

      "Registered names have an Int representation" in {
        assert(Port(80).intValue === 80)
      }

      "port = *DIGIT" in {
        intercept[IllegalArgumentException](Port(-13))
      }

    }

    """Authority user friendly constructors""" in {
      val hostName = "www.example.com"
      val port = 8080
      val userInfo = "JohnDoe"
      assert(Authority(Host(hostName)) === Authority(None, Host(hostName), None))
      assert(Authority(hostName) === Authority(None, Host(hostName), None))
      assert(Authority(UserInfo(userInfo), Host(hostName)) === Authority(Some(UserInfo(userInfo)), Host(hostName), None))
      assert(Authority(userInfo, hostName) === Authority(Some(UserInfo(userInfo)), Host(hostName), None))
      assert(Authority(Host(hostName), Port(port)) === Authority(None, Host(hostName), Some(Port(port))))
      assert(Authority(hostName, port) === Authority(None, Host(hostName), Some(Port(port))))
      assert(Authority(UserInfo(userInfo), Host(hostName), Port(port)) === Authority(Some(UserInfo(userInfo)), Host(hostName), Some(Port(port))))
      assert(Authority(userInfo, hostName, port) === Authority(Some(UserInfo(userInfo)), Host(hostName), Some(Port(port))))
    }

    """Authority has a string representation""" - {

      val hostName = "www.example.com"
      val port = 8080

      "Simple authority" in {
        val userInfo = "JohnDoe"
        assert(Authority(hostName).stringValue === hostName)
        assert(Authority(userInfo, hostName).stringValue === s"$userInfo@$hostName")
        assert(Authority(hostName, port).stringValue === s"$hostName:$port")
        assert(Authority(userInfo, hostName, port).stringValue === s"$userInfo@$hostName:$port")
      }

      "Escape '@' in userinfo" in {
        assert(Authority("john@doe.es", hostName).stringValue === s"john%40doe.es@$hostName")
      }

      "Escape ':' in host so that ':' delimits the port" in {
        assert(Authority("example.com:12", port).stringValue === "example.com%3A12:8080")
      }

      "Escape ':' in userinfo to distinguish it from user:password deprecated format" in {
        assert(Authority("john:doe", hostName).stringValue === s"john%3Adoe@$hostName")
      }

    }
  }

}
