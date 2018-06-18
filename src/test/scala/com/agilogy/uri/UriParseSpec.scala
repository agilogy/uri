package com.agilogy.uri

import org.scalatest.{ EitherValues, FreeSpec, Matchers, TryValues }
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import UriGenerators._
import validation.Validation._

class UriParseSpec extends FreeSpec with GeneratorDrivenPropertyChecks with Matchers with TryValues with EitherValues {

  "Simple case" in {
    val uri = Uri(Scheme("http").right.value, Authority(Some(UserInfo("쏦⥺맃꙽䐪%")), Host("阮䳐똦ꉫ⋃切鱙뻟➥ᘟ㡚"), Some(Port(49514).right.value)),
      Path / "፷覀뎳ﹸ夏偩" / "듃㓯音ꨈ꾥䳊⻋吜" / "䧝䉟鮇鄽" / "ꝯ閵Ũ㊸" / "䵇ी᦮鰏莞" / "鈇ൃ퐗㥝▀ꏥኑ嚷뛿" / "䱩" / "䙞튆" / "" / "ⰶ겏ﱅ⺬펯䃙" / "쑢◚잲", None, None)
    assert(Uri.parseTry(uri.stringValue).get === uri)
  }

  "Segment case" in {
    val uri = Uri(Scheme("http").right.value, Authority("localhost"),Path./(NonEmptySegment("ίTb")))
    val parsed = Uri.parse(uri.stringValue)
    assert(parsed.right.value === uri)
  }

  "Parse a path" in {
    val p = Path("a b").right.value / "b"
    assert(p.stringValue === "a%20b/b")
    assert(Path.parse(p.stringValue) === p)
  }

  "Parse  the string value of an authority" in {
    forAll(authorities)(a => Authority.parseTry(a.stringValue).success.value should equal(a))
  }

  "Parse the string value of a uri" in {
    forAll(uris) {
      u =>
        //        println(s"Testing ${u.stringValue}")
        Uri.parseTry(u.stringValue).success.value should equal(u)
        Uri.parseTry(u.asciiStringValue).success.value should equal(u)
    }
  }

  "Uri validation should report parse errors" - {

    "missing scheme" in {
      val uri = "/foo"
      val expectedFailure = List(MissingScheme(uri))
      val res = Uri.parse(uri)
      assert(res.isLeft)
      assert(res.left.value === expectedFailure)
      val tryRes = Uri.parseTry(uri)
      assert(tryRes.isFailure)
      assert(tryRes.failure.exception === ValidationFailure(expectedFailure))
    }

    //    "foo" in {
    //      val res = Uri.parse("http://#")
    //      println(res)
    //      assert(res.isLeft)
    //    }

  }

}
