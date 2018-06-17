package com.agilogy.uri

import org.scalatest.{ FreeSpec, TryValues, _ }
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import UriGenerators._

class UriJavaUriSpec extends FreeSpec with GeneratorDrivenPropertyChecks with Matchers with TryValues {

  //  implicit override val generatorDrivenConfig = PropertyCheckConfiguration(maxDiscarded = 2000)

  """Single case""" in {
    //    val u = CompleteUri(Scheme("v5v"),None,Some(RootlessPath(Segment("&b!#"))),None,None)
    //    val u = CompleteUri(Scheme("v5v"),None,Some(RootlessPath(Segment("abc"))),None,None)
    //    val u = CompleteUri(Scheme("foo"),None,Some(RootlessPath(Segment("}iV쵑"))), None, None)
    //    assert(u.authority.isEmpty)
    val u = Uri.of(Scheme("r+"), Some(Authority(Some(UserInfo("1gs")), Host("-$"), None)), Path.absolute(Segment("F;c"), Segment("𥳐L2*-2]¤S"), Segment("T=l"), Segment("_,齈wb/"), Segment("L,[")), None, None)
    val ju = u.toJava
    val su = ju.toString
    assert(new java.net.URI(su) === ju)
    assert(Uri.parseTry(su).success.value === u, s"""su = "$su" """)
  }

  """Represent URI like java.net.URI""" in {
    forAll(uris) {
      u =>
        if (!u.path.isEmpty || u.query.nonEmpty || u.fragment.nonEmpty || u.authority.exists(_.host.stringValue.nonEmpty)) {
          val ju = u.toJava
          val su = ju.toString
          assert(new java.net.URI(su) === ju)
          assert(Uri.parseTry(su).success.value === u, s"""su = "$su" """)
        }
    }
  }
}
