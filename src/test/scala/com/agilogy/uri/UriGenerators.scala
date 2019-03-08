package com.agilogy.uri

import org.scalacheck.{ Arbitrary, Gen }

object UriGenerators {

  private val utfChars = implicitly[Arbitrary[Char]].arbitrary

  private val chars: Gen[Char] = for {
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

  private def alphaStr(maxSize: Int, minSize: Int = 0): Gen[String] = for {
    size <- Gen.choose(minSize, maxSize)
    list <- Gen.listOfN(size, chars)
  } yield list.mkString

  //Gen.listOfN(maxSize,Gen.alphaChar).map(_.mkString).suchThat(_.forall(_.isLetter))

  private val schemeChars = Gen.frequency(9 -> Gen.alphaNumChar, 1 -> Gen.oneOf('+', '-', '.'))

  private val schemes = for {
    c <- Gen.alphaChar
    size <- Gen.choose(0, 5)
    list <- Gen.listOfN(size, schemeChars)
  } yield Scheme(c + list.mkString("")).right.get

  private val userInfos = for (s <- alphaStr(10)) yield UserInfo(s)

  private val registeredNames = for {
    s <- alphaStr(20) //if Encoder.isValidRegisteredName(s)
  } yield Host(s)

  private val ports = Gen.chooseNum(1, 65000).map(i => Port(i).right.get)

  private val nonEmptySegments = for (s <- alphaStr(10, 1)) yield NonEmptySegment(s)

  private val segments: Gen[Segment] = Gen.frequency(1 -> Gen.const(Segment.Empty), 9 -> nonEmptySegments)

  private lazy val absolutePaths: Gen[PathAbEmpty] = {
    Gen.sized { size =>
      for {
        size <- Gen.choose(0, size)
        segmentList <- Gen.listOfN(size, segments)
      } yield Path.absoluteOrEmpty(segmentList: _*)
    }
  }

  private lazy val rootlessPaths: Gen[RootlessPath] = {
    Gen.sized { size =>
      for {
        head <- nonEmptySegments
        tailSize <- Gen.choose(0, size)
        segmentList <- Gen.listOfN(tailSize, segments)
      } yield segmentList.foldLeft[RootlessPath](Path(head))(_ / _)
    }
  }

  private lazy val paths: Gen[Path] = Gen.oneOf(absolutePaths, rootlessPaths)

  val authorities: Gen[Authority] = for {
    ui <- Gen.option(userInfos)
    h <- registeredNames
    p <- Gen.option(ports)
  } yield Authority(ui, h, p)

  private val queries = alphaStr(10).map(Query.apply)

  private val fragments = alphaStr(6).map(Fragment.apply)

  private val authorityUris = for {
    s <- schemes
    a <- authorities
    p <- absolutePaths
    q <- Gen.option(queries)
    f <- Gen.option(fragments)
  } yield RichUri(s, a, p, q, None)

  private val authoritylessUris = for {
    s <- schemes
    p <- paths if !p.stringValue.startsWith("//")
    q <- if (p.isEmpty) queries.map(Some.apply) else Gen.option(queries)
    f <- Gen.option(fragments)
  } yield RichUri.noAuthority(s, p, q, None).right.get

  val uris: Gen[Uri] = Gen.frequency(9 -> authorityUris, 1 -> authoritylessUris)

}
