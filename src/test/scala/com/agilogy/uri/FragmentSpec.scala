package com.agilogy.uri

import org.scalatest.FreeSpec

class FragmentSpec extends FreeSpec{

  """
    |The fragment identifier component of a URI allows indirect identification of a secondary resource by reference to
    |a primary resource and additional identifying information.""".stripMargin - {

    "Fragment identifier components have a string representation" in {
      assert(Fragment("Address").stringValue === "Address")
    }

    """Under normal circumstances, the only time when octets within a URI are percent-encoded is during the process of
      |producing the URI from its component parts.""".stripMargin in {
      assert(Fragment(":/?#%").stringValue === ":/?#%")
    }


  }

}
