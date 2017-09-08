package com.agilogy.uri

import org.scalatest.FreeSpec

class QuerySpec extends FreeSpec {

  """
    |The query component contains non-hierarchical data that, along with data in the path component (Section 3.3),
    |serves to identify a resource within the scope of the URI's scheme and naming authoritym (if any). """
    .stripMargin - {

      "Query components have a string representation" in {
        assert(Query("name=John").stringValue === "name=John")
      }

      """Under normal circumstances, the only time when octets within a URI are percent-encoded is during the process of
      |producing the URI from its component parts.""".stripMargin in {
        assert(Query(":/?#%").stringValue === ":/?#%")
      }

    }

}
