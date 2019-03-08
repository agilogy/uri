package com.agilogy.uri

import validation.Validation.{notNull, sequence}

object UriParser {

  def matchParts(uri: String): (Option[String], Option[String], Option[String], Option[String], Option[String]) = {

    val chars = uri.toCharArray
    val length = chars.length
    var i = 0

    def readUntil(delimiters: Set[Char]): String = {
      if (i >= length) ""
      else {
        val res = StringBuilder.newBuilder
        while (i < length && !delimiters.contains(chars(i))) {
          res.append(chars(i))
          i += 1
        }
        res.toString()
      }
    }

    def read(): String = {
      if (i >= length) ""
      else {
        val res = StringBuilder.newBuilder
        while (i < length) {
          res.append(i)
          i += 1
        }
        res.toString()
      }
    }

    var scheme: Option[String] = Some(readUntil(Set(':', '/', '?', '#')))
    if (i < length && chars(i) == ':') {
      i += 1
    } else {
      scheme = None
      i = 0
    }
    val authority = {
      if (i + 1 < length && chars(i) == '/' && chars(i + 1) == '/') {
        i = i + 2
        Some(readUntil(Set('/', '?', '#')))
      } else {
        None
      }
    }
    val path = if (i < length && !Set('?', '#').contains(chars(i))) {
      Some(readUntil(Set('?', '#')))
    } else {
      None
    }

    val query = if (i < length && chars(i) == '?') {
      i += 1
      Some(readUntil(Set('#')))
    } else {
      None
    }

    val fragment = if (i < length && chars(i) == '#') {
      i += 1
      Some(read())
    } else {
      None
    }

    (scheme, authority, path, query, fragment)
  }

  def parseUriParts(value: String): Either[UriParseError, (Scheme, Option[Authority], Option[String], Option[String], Option[String])] = {
    matchParts(value) match {
      case (s, a, p, q, f) =>
        val scheme: Either[SchemeError, Scheme] = notNull(MissingScheme(value), s).flatMap(Scheme.apply)
        val authority: Either[AuthorityParseError, Option[Authority]] = sequence(a.map(Authority.parse))
        (scheme, authority) match {
          case (Right(rs), Right(ra)) =>
            // The parts matcher makes it impossible for a PathError to occur
            Right((rs, ra, p, q, f))
          case _ =>
            Left(UriParseError(scheme.left.toOption, authority.left.toOption))
        }
      case _ =>
        // It actually never happens. Every possible string matches the parts
        throw new IllegalStateException("Unreachable code")
    }
  }

  def parseUriPacked(value: String): Either[UriParseError, PackedUri] = parseUriParts(value).map(_ => PackedUri(value))

  def parseUriRich(value: String): Either[UriParseError, RichUri] = parseUriParts(value).map {
    case (s, a, p, q, f) =>
      val path = Path.parse(p.getOrElse(""))
      val query = q.map(q => Query(Encoder.decode(q)))
      val fragment = f.map(f => Fragment(Encoder.decode(f)))
      // The parts matcher makes it impossible for a PathError to occur
      Uri.of(s, a, path, query, fragment).right.get
  }


}
