package com.agilogy.uri

import org.scalacheck.{ Arbitrary, Gen }

object UriGenerators {

  val utfChars = implicitly[Arbitrary[Char]].arbitrary

  val chars: Gen[Char] = for {
    option <- Gen.frequency(90 -> "a", 7 -> "l", 3 -> "o")
    c <- option match {
      case "a" =>
        // See https://en.wikipedia.org/wiki/UTF-8
        Gen.choose(20.toChar, 126.toChar)
      case "l" =>
        // See https://en.wikipedia.org/wiki/Latin-1_Supplement_(Unicode_block)
        Gen.choose(0x00a1.toChar, 0x00ff.toChar)
      case "o" => utfChars.filter(_.getType != Character.FORMAT)
    }
  } yield c

  def alphaStr(maxSize: Int, minSize: Int = 0): Gen[String] = for {
    size <- Gen.choose(minSize, maxSize)
    list <- Gen.listOfN(size, chars)
  } yield list.mkString

  //Gen.listOfN(maxSize,Gen.alphaChar).map(_.mkString).suchThat(_.forall(_.isLetter))

  private val schemeChars = Gen.frequency(9 -> Gen.alphaNumChar, 1 -> Gen.oneOf('+', '-', '.'))

  val schemes = for {
    c <- Gen.alphaChar
    size <- Gen.choose(0, 5)
    list <- Gen.listOfN(size, schemeChars)
  } yield Scheme(c + list.mkString(""))

  val userInfos = for (s <- alphaStr(10)) yield UserInfo(s)

  val registeredNames = for {
    s <- alphaStr(20) //if Encoder.isValidRegisteredName(s)
  } yield RegisteredName(s)

  val ports = Gen.chooseNum(1, 65000).map(Port.apply)

  val nonEmptySegments = for (s <- alphaStr(10, 1)) yield Segment(s)

  val segments: Gen[Segment] = Gen.frequency(1 -> Gen.const(Segment.Empty), 9 -> nonEmptySegments)

  lazy val absolutePaths: Gen[PathAbEmpty] = {
    Gen.sized { size =>
      for {
        size <- Gen.choose(0, size)
        segmentList <- Gen.listOfN(size, segments)
      } yield Path.absoluteOrEmpty(segmentList: _*)
    }
  }

  lazy val rootlessPaths: Gen[RootlessPath] = {
    Gen.sized { size =>
      for {
        head <- nonEmptySegments
        tailSize <- Gen.choose(0, size)
        segmentList <- Gen.listOfN(tailSize, segments)
      } yield segmentList.foldLeft[RootlessPath](RootlessSingleSegmentPath(head))(_ / _)
    }
  }

  lazy val paths: Gen[Path] = Gen.oneOf(absolutePaths, rootlessPaths)

  val authorities = for {
    ui <- Gen.option(userInfos)
    h <- registeredNames
    p <- Gen.option(ports)
  } yield Authority(ui, h, p)

  val queries = alphaStr(10).map(Query.apply)

  val fragments = alphaStr(6).map(Fragment.apply)

  val authorityUris = for {
    s <- schemes
    a <- authorities
    p <- absolutePaths
    q <- Gen.option(queries)
    f <- Gen.option(fragments)
  } yield CompleteUri(s, Some(a), p, q, None)

  val authoritylessUris = for {
    s <- schemes
    p <- paths if !p.stringValue.startsWith("//")
    q <- if (p.isEmpty) queries.map(Some.apply) else Gen.option(queries)
    f <- Gen.option(fragments)
  } yield CompleteUri(s, None, p, q, None)

  val uris = Gen.frequency(9 -> authorityUris, 1 -> authoritylessUris)

}
