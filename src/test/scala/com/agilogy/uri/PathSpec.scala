package com.agilogy.uri

import org.scalatest.FlatSpec

class PathSpec extends FlatSpec {

  behavior of "Path.Empty"

  it should "build have a string representation" in {
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

  it should "compose paths with /" in {
    assert(Path.Empty / Path.Absolute("a", Path.Empty) === Path.Absolute("a", Path.Empty))
    assert(Path.Empty / Path.Relative("a", Path.Empty) === Path.Absolute("a", Path.Empty))
    assert(Path.Empty / Path.Slash === Path.Slash)
    assert(Path.Empty / Path.Empty === Path.Slash)
  }

  behavior of "/"

  it should "have a string representation" in {
    assert(Path.Slash.value === "/")
  }

  it should "parse parsed from a string" in {
    assert(Path("/") === Path.Slash)
  }

  it should "compose paths with /" in {
    assert(Path.Slash / Path.Absolute("a", Path.Empty) === Path.Absolute("a", Path.Empty))
    assert(Path.Slash / Path.Relative("a", Path.Empty) === Path.Absolute("a", Path.Empty))
    assert(Path.Slash / Path.Slash === Path.Slash)
    assert(Path.Slash / Path.Empty === Path.Slash)
  }

  behavior of "AbsolutePath"

  it should "have a string representation" in {
    assert(Path.Absolute("departments", Path.Absolute("1", Path.Empty)).value === "/departments/1")
    assert(Path.Absolute("departments", Path.Absolute("1", Path.Slash)).value === "/departments/1/")
  }

  it should "be parsed from a string" in {
    assert(Path("/departments/1") === Path.Absolute("departments", Path.Absolute("1", Path.Empty)))
    assert(Path("/departments/1/") === Path.Absolute("departments", Path.Absolute("1", Path.Slash)))
  }

  it should "compose paths with /" in {
    val depts = Path.Absolute("departments", Path.Slash)
    val dept1 = Path.Absolute("departments", Path.Absolute("1", Path.Empty))
    assert(dept1 / Path.Absolute("a", Path.Empty) === Path.Absolute("departments", Path.Absolute("1", Path.Absolute("a", Path.Empty))))
    assert(dept1 / Path.Relative("a", Path.Empty) === Path.Absolute("departments", Path.Absolute("1", Path.Absolute("a", Path.Empty))))
    assert(dept1 / Path.Slash === Path.Absolute("departments", Path.Absolute("1", Path.Slash)))
    assert(dept1 / Path.Empty === Path.Absolute("departments", Path.Absolute("1", Path.Slash)))

    assert(depts / Path.Absolute("a", Path.Empty) === Path.Absolute("departments", Path.Absolute("a", Path.Empty)))
    assert(depts / Path.Relative("a", Path.Empty) === Path.Absolute("departments", Path.Absolute("a", Path.Empty)))
    assert(depts / Path.Slash === depts)
    assert(depts / Path.Empty === depts)
  }

  behavior of "RelativePath"

  it should "have a string representation" in {
    assert(Path.Relative("departments", Path.Absolute("1", Path.Empty)).value === "departments/1")
    assert(Path.Relative("departments", Path.Absolute("1", Path.Slash)).value === "departments/1/")
  }

  it should "be parsed from a string" in {
    assert(Path("departments/1") === Path.Relative("departments", Path.Absolute("1", Path.Empty)))
    assert(Path("departments/1/") === Path.Relative("departments", Path.Absolute("1", Path.Slash)))
  }

  it should "compose paths with /" in {
    val depts = Path.Relative("departments", Path.Slash)
    val dept1 = Path.Relative("departments", Path.Absolute("1", Path.Empty))
    assert(dept1 / Path.Absolute("a", Path.Empty) === Path.Relative("departments", Path.Absolute("1", Path.Absolute("a", Path.Empty))))
    assert(dept1 / Path.Relative("a", Path.Empty) === Path.Relative("departments", Path.Absolute("1", Path.Absolute("a", Path.Empty))))
    assert(dept1 / Path.Slash === Path.Relative("departments", Path.Absolute("1", Path.Slash)))
    assert(dept1 / Path.Empty === Path.Relative("departments", Path.Absolute("1", Path.Slash)))

    assert(depts / Path.Absolute("a", Path.Empty) === Path.Relative("departments", Path.Absolute("a", Path.Empty)))
    assert(depts / Path.Relative("a", Path.Empty) === Path.Relative("departments", Path.Absolute("a", Path.Empty)))
    assert(depts / Path.Slash === depts)
    assert(depts / Path.Empty === depts)
  }



}
