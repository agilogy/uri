package com.agilogy.uri

import org.scalatest.FreeSpec

class SchemeSpec extends FreeSpec {

  """
    |Each URI begins with a scheme name that refers to a specification for assigning identifiers within that scheme.
    |As such, the URI syntax is a federated and extensible naming system wherein each scheme's specification may further
    |restrict the syntax and semantics of identifiers using that scheme.""".stripMargin - {

    "Schemes have a string representation" in {
      assert(Scheme("http").stringValue === "http")
    }

//    """Under normal circumstances, the only time when octets within a URI are percent-encoded is during the process of
//      |producing the URI from its component parts.""".stripMargin in {
//      assert(Scheme(":/?#%").stringValue === ":/?#%")
//    }

    """Although schemes are case-insensitive, the canonical form is lowercase and documents that specify schemes must do
      |so with lowercase letters.  An implementation should accept uppercase letters as equivalent to lowercase in scheme
      |names (e.g., allow "HTTP" as well as "http") for the sake of robustness but should only produce lowercase scheme
      |names for consistency.""".stripMargin in {
      assert(Scheme("HttP").stringValue === "http")
//      assert(Scheme("ÀÇ").stringValue === "àç")
    }

    //  """When presented with a URI that violates one or more scheme-specific restrictions, the scheme-specific resolution
    //    |process should flag the reference as an error""".stripMargin in {
    //  }
  }

}
