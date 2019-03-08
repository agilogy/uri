package com.agilogy.uri

import com.agilogy.uri.UriGenerators._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, TryValues, _}

class UriJavaUriSpec extends FreeSpec with GeneratorDrivenPropertyChecks with Matchers with TryValues with EitherValues {


  """Single case""" in {
    val u = Uri.of(Scheme("r+").right.value, Some(Authority(Some(UserInfo("1gs")), Host("-$"), None)), Path.absolute(Segment("F;c"), Segment("𥳐L2*-2]¤S"), Segment("T=l"), Segment("_,齈wb/"), Segment("L,[")), None, None).right.value
    val ju = u.toJava
    val su = ju.toString
    assert(new java.net.URI(su) === ju)
    assert(Uri.parseTryRich(su).success.value === u, s"""su = "$su" """)
  }

  """Represent URI like java.net.URI""" in {
    forAll(uris) {
      u =>
        if (!u.path.isEmpty || u.query.nonEmpty || u.fragment.nonEmpty || u.authority.exists(_.host.stringValue.nonEmpty)) {
          val ju = u.toJava
          val su = ju.toString
          assert(new java.net.URI(su) === ju)
          assert(Uri.parseTryRich(su).success.value === u, s"""su = "$su" """)
        }
    }
  }
}
