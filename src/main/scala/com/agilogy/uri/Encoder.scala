package com.agilogy.uri

import java.text.Normalizer

import scala.util.{Failure, Success, Try}

object Encoder {
  private val subDelims = "!$&'()*+,;=".toSet
  private val unreserved = "-._~".toSet
  private val schemeChars = "+-.".toSet
  private val userInfoChars = unreserved ++ subDelims ++ Set(':')
  private val registeredNameChars = unreserved ++ subDelims
  private val pathSegmentChars = unreserved ++ subDelims ++ ":@".toSet
  private val queryChars = pathSegmentChars ++ "/?".toSet

  private def pctEncode(c: Char): String = {
    if (c <= '\u000F') {
      "%0" + Integer.toHexString(c.toInt).toUpperCase
    } else if (c <= '\u0080') {
      "%" + Integer.toHexString(c.toInt).toUpperCase
    } else {
      Try(JavaNetCodec.encode(c.toString)) match {
        case Success(s) => s
        case Failure(t) =>
          throw new RuntimeException(t)
      }
    }
  }

  @inline private def isAlpha(c: Char) = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

  @inline private def isDigit(c: Char) = c >= '0' && c <= '9'

  private def isAlphaNum(c: Char): Boolean = isAlpha(c) || isDigit(c)

  private def isValidSchemeChar(c: Char): Boolean = isAlphaNum(c) || schemeChars.contains(c)

  def isValidScheme(v: String): Boolean = {
    v != null && v.length > 0 && isAlpha(v.charAt(0)) && v.forall(isValidSchemeChar)
  }

  def asciiEncode(s: String): String = {
    JavaNetCodec.encode(s)
  }

  def quote(s: String, allowedChars: Set[Char]): String = {
    val sb = new StringBuilder
    val i = s.iterator
    while (i.hasNext) {
      val c = i.next()
      if (c.isHighSurrogate && i.hasNext) {
        val l = i.next()
        if (!l.isLowSurrogate) {
          sb.append(pctEncode(l))
        } else {
          // e.g. 𥳐 = '\uD857' + '\uDCD0'
          sb.append(JavaNetCodec.encode("" + c + l))
        }
      } else if (c.isLetterOrDigit || allowedChars.contains(c)) {
        sb.append(c)
      } else {
        sb.append(pctEncode(c))
      }
    }
    sb.toString()
  }

  def decode(s: String): String = {
    JavaNetCodec.decode(s)
  }

  def quoteAuthority(a: Authority): String = {
    a.userInfo.fold("")(ui => Encoder.quote(ui.stringValue, Encoder.userInfoChars -- Set(':')) + "@") +
      Encoder.quote(a.host.stringValue, Encoder.registeredNameChars) +
      a.port.fold("")(":" + _.stringValue)
  }

  def quoteSegment(s: Segment): String = {
    Encoder.quote(s.stringValue, Encoder.pathSegmentChars)
  }

  def quoteUri(uri: Uri): String = {
    val sScheme = uri.scheme.stringValue
    val sAuthority = uri.authority.map(a => "//" + a.stringValue).getOrElse("")
    val sPath = uri.path.stringValue
    val sQuery = uri.query.map(q => "?" + Encoder.quote(q.stringValue, Encoder.queryChars)).getOrElse("")
    val sFragment = uri.fragment.map(f => "#" + f.stringValue).getOrElse("")
    s"$sScheme:$sAuthority$sPath$sQuery$sFragment"
  }

  def normalize(s: String): String = Normalizer.normalize(s, Normalizer.Form.NFC)

}
