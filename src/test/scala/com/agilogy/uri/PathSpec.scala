package com.agilogy.uri

import org.scalatest.{EitherValues, FreeSpec}
import validation.Validation._

//TODO: Test Path.parse
//TODO: Test Path.parse("")
class PathSpec extends FreeSpec with EitherValues{

  """
    |The path component contains data, usually organized in hierarchical form, that, along with data in the
    |non-hierarchical query component (Section 3.4), serves to identify a resource within the scope of the URI's scheme
    |and naming authority (if any).""".stripMargin - {

    """A path consists of a sequence of path segments separated by a slash ("/") character.""" - {

      "Segments have a string representation" in {
        assert(Segment("Posts").stringValue === "Posts")
      }

      """Under normal circumstances, the only time when octets within a URI are percent-encoded is during the process of
        |producing the URI from its component parts.""".stripMargin in {
        assert(Segment(":/?#%").stringValue === ":/?#%")
      }
    }

    """path = path-abempty    ; begins with "/" or is empty
      |     / path-absolute   ; begins with "/" but not "//"
      |     / path-noscheme   ; begins with a non-colon segment
      |     / path-rootless   ; begins with a segment
      |     / path-empty      ; zero characters""".stripMargin - {

      "path-absolute" in {
        assert(Path.Slash.segments === Seq(Segment.Empty))
        assert(Path.Slash.isAbsolute === true)
        val slashPosts = Path / Segment("Posts")
        assert(Path / "Posts" === slashPosts)
        assert(slashPosts.segments === Seq(Segment("Posts")))
        assert(slashPosts.isAbsolute === true)
        val post23 = slashPosts / "23"
        assert(post23.isAbsolute === true)
        assert(post23.segments === Seq(Segment("Posts"), Segment("23")))
      }

      "path-rootless" in {
        val postsAttempt = Path("Posts")
        val posts = postsAttempt.right.value
        assert(posts.isAbsolute === false)
        assert(posts.segments === Seq(Segment("Posts")))

        val post23 = posts / "23"
        assert(post23.isAbsolute === false)
        assert(post23.segments === Seq(Segment("Posts"), Segment("23")))

        assert((postsAttempt / "23").right.value === post23)
      }

      "path-rootless with errors" in {
        val posts = Path("")
        assert(posts === Left(FirstSegmentIsEmptyInRootlessPath))
        val err = posts / "23"
        assert(err === Left(FirstSegmentIsEmptyInRootlessPath))
      }
    }

    """A path has a string representation""" - {

      """Simple paths""" in {
        assert(Path.Slash.stringValue === "/")
        assert((Path / "a" / "b").stringValue === "/a/b")
        assert((Path("a").right.value / "b").stringValue === "a/b")
      }

      """Encode '/' so that the segments can be identified""" in {
        assert((Path / "/" / "a").stringValue === "/%2F/a")
        assert((Path("/").right.value / "a").stringValue === "%2F/a")
      }
    }

  }

}
