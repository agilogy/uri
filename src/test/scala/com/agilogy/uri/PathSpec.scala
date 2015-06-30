package com.agilogy.uri

import org.scalatest.FlatSpec

class PathSpec extends FlatSpec {

  behavior of "Empty"

  it should "have a string representation" in {
    assert(Path.Empty.value === "")
  }

  it should "be parsed from a string" in {
    assert(Path("") === Path.Empty)
  }

  it should "pattern match" in {
    Path("") match {
      case Path.Empty => //nop
      case _ => fail("Should match Path.Empty")
    }
  }

  behavior of "Slash"

  it should "have a string representation" in {
    assert(Path.Slash.value === "/")
  }

  it should "be parsed from a string" in {
    assert(Path("/") === Path.Slash)
  }

  it should "pattern match" in {
    Path("/") match {
      case Path.Slash => //nop
      case _ => fail("Should match Path.Slash")
    }
  }

  it should "compose paths with /" in {
    assert((Path.Slash / "a").value === "/a")
    assert((Path.Slash / "").value === "/")
  }

  behavior of "Absolute"

  it should "have a string representation" in {
    assert((Path.Slash / "departments").value === "/departments")
  }

  it should "be parsed from a string" in {
    assert(Path("/departments/1") === Path.Slash / "departments" / "1")
    assert(Path("/departments/1/") === Path.Slash / "departments" / "1" / "")
  }

  it should "pattern match" in {
    Path("/departments/23") match {
      case Path.Slash / "departments" / d => assert(d === "23")
      case _ => fail("Should match Path.Slash / departments / id")
    }
    Path("/departments/foo/") match {
      case Path.Slash / d / "foo" / "" => assert(d === "departments")
      case _ => fail("Should pattern match")
    }
  }

  it should "compose paths with /" in {
    val depts = Path.Slash / "departments" / ""
    val dept1 = Path.Slash / "departments" / "1"
    assert((dept1 / "a").value === "/departments/1/a")
    assert((dept1 / "").value === "/departments/1/")

    assert((depts / "a").value === "/departments/a")
    assert((depts / "").value === "/departments/")
  }

  behavior of "Base"

  it should "have a string representation" in {
    assert(Path.Base.value === ".")
  }

  it should "be parsed from a string" in {
    assert(Path(".") === Path.Base)
  }

  it should "pattern match" in {
    Path(".") match {
      case Path.Base => //nop
      case _ => fail("Should match Path.Base")
    }
  }

  it should "compose paths with /" in {
    assert((Path.Base / "departments").value === "departments")
  }

  behavior of "Relative"

  it should "have a string representation" in {
    assert((Path.Base / "departments").value === "departments")
  }

  it should "be parsed from a string" in {
    assert(Path("departments/1") === Path.Base / "departments" / "1")
    assert(Path("departments/1/") === Path.Base / "departments" / "1" / "")
  }

  it should "pattern match" in {
    Path("departments/23") match {
      case Path.Base / "departments" / d => assert(d === "23")
      case _ => fail("Should match Path.Base / departments / id")
    }
    Path("departments/foo/") match {
      case Path.Base / d / "foo" / "" => assert(d === "departments")
      case _ => fail("Should pattern match")
    }
  }


  it should "compose paths with /" in {
    val depts = Path.Base / "departments" / ""
    val dept1 = Path.Base / "departments" / "1"
    assert((dept1 / "a").value === "departments/1/a")
    assert((dept1 / "").value === "departments/1/")

    assert((depts / "a").value === "departments/a")
    assert((depts / "").value === "departments/")
  }

  behavior of ".."

  it should "work with absolute paths" in {
    assert(Path("/departments/23/..").value === "/departments")
    assert(Path("/departments/23/../12").value === "/departments/12")
    assert(Path("/departments/23/../12") === Path.Slash / "departments" / "23" / ".." / "12")
    assert(Path("/departments/23/../..").value === "/")
    assert(Path("/departments/23/../../../..").value === "/")
  }

  it should "work with relative paths" in {
    assert(Path("departments/23/..").value === "departments")
    assert(Path("departments/23/../12").value === "departments/12")
    assert(Path("departments/23/../12") === Path.Base / "departments" / "23" / ".." / "12")
    assert(Path("departments/23/../..").value === ".")
    assert(Path("departments/23/../../../..").value === "../..")
  }

  behavior of "."

  it should "work with absolute paths" in {
    assert(Path("/departments/23/./12") === Path.Slash / "departments" / "23" / "." / "12")
    assert(Path("/departments/23/.").value === "/departments/23")
    assert(Path("/departments/23/./").value === "/departments/23/")
    assert(Path("/departments/23/./12").value === "/departments/23/12")
    assert(Path("/departments/././23").value === "/departments/23")
    assert(Path("/departments/23/./././.").value === "/departments/23")
  }

  it should "work with relative paths" in {
    assert(Path("departments/23/./12") === Path.Base / "departments" / "23" / "." / "12")
    assert(Path("departments/23/.").value === "departments/23")
    assert(Path("departments/23/./").value === "departments/23/")
    assert(Path("departments/23/./12").value === "departments/23/12")
    assert(Path("departments/././23").value === "departments/23")
    assert(Path("departments/23/./././.").value === "departments/23")
    assert(Path.Base / "." === Path.Base)
  }

}
