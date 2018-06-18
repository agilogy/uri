package com.agilogy.uri

import java.text.Normalizer

import scala.util.{ Failure, Success, Try }

object Encoder {
  //  private val genDelims = ":/?#[]@".toSet
  private val subDelims = "!$&'()*+,;=".toSet
  //  private val reserved = genDelims ++ subDelims
  private val unreserved = "-._~".toSet
  private val schemeChars = "+-.".toSet
  private val userInfoChars = unreserved ++ subDelims ++ Set(':')
  private val registeredNameChars = unreserved ++ subDelims
  private val pathSegmentChars = unreserved ++ subDelims ++ ":@".toSet
  private val queryChars = pathSegmentChars ++ "/?".toSet
  //  private val fragmentChars = queryChars
  //  val allChars = reserved ++ unreserved ++ schemeChars ++ " %".toSet

  private def pctEncode(c: Char): String = {
    if (c <= '\u000F') {
      "%0" + Integer.toHexString(c.toInt).toUpperCase
    } else if (c <= '\u0080') {
      "%" + Integer.toHexString(c.toInt).toUpperCase
    } else {
      Try(JavaNetCodec.encode(c.toString)) match {
        case Success(s) => s
        case Failure(t) =>
//          println(s"ERROR encoding $c")
//          throw new RuntimeException(t)
          "?"

      }
    }
    //    "%" + Integer.toHexString(c.toInt).toUpperCase
  }

  //  implicit class CharOps(c:Char){
  //    def encode:String = "%" + Integer.toHexString(c.toInt).toUpperCase
  //  }

  @inline private def isAlpha(c: Char) = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

  @inline private def isDigit(c: Char) = c >= '0' && c <= '9'

  private def isAlphaNum(c: Char): Boolean = isAlpha(c) || isDigit(c)

  private def isValidSchemeChar(c: Char): Boolean = isAlphaNum(c) || schemeChars.contains(c)

  def isValidScheme(v: String): Boolean = {
    v != null && v.length > 0 && isAlpha(v.charAt(0)) && v.forall(isValidSchemeChar)
  }

  //  def isValidRegisteredNameChar(c:Char):Boolean = c.isLetterOrDigit || registeredNameChars.contains(c)

  //  def isValidRegisteredName(v:String):Boolean = {
  //    v != null && v.forall(isValidRegisteredNameChar)
  //  }

  def asciiEncode(s: String): String = {
    JavaNetCodec.encode(s)
  }

  def quote(s: String, allowedChars: Set[Char]): String = {
    //    val forbiddenChars = allChars -- allowedChars
    val sb = new StringBuilder
    //    var hs: Char = 0.toChar
    val i = s.iterator
    while (i.hasNext) {
      val c = i.next()
      if (c.isHighSurrogate && i.hasNext) {
        //      } else if (c.isHighSurrogate) {
        //          if (!i.hasNext) {
//        sb.append(pctEncode(c))
        //          throw new RuntimeException(s"Unexpected low surrogate ${Integer.toHexString(c.toInt)}")
        //          } else {
        val l = i.next()
        if (!l.isLowSurrogate) {
          //            throw new IllegalArgumentException(s"Unexpected char $c (${Integer.toHexString(l.toInt)}) after high surrogate ${Integer.toHexString(c.toInt)}")
          sb.append(pctEncode(l))
        } else {
          // e.g. 𥳐 = '\uD857' + '\uDCD0'
          sb.append(JavaNetCodec.encode("" + c + l))
        }
        //          }
      } else if (c.isLetterOrDigit || allowedChars.contains(c)) {
          // (isAlphaNum(c) || allowedChars.contains(c)) {
          sb.append(c)
          //      } else if (c.isLowSurrogate) {
          //        throw new RuntimeException(s"Unexpected low surrogate ${Integer.toHexString(c.toInt)}")
      } else {
        sb.append(pctEncode(c))
      }
    }
    sb.toString()
  }

  //  private def pctDecodeChar(c:String):Char = {
  //    if (c.length == 1) {
  //      c.charAt(0)
  //    } else if (c.startsWith("%")) {
  //      Integer.parseInt(c.substring(1), 16).toChar
  //    } else {
  //      throw new IllegalArgumentException(s"Can't decode $c")
  //    }
  //  }

  def decode(s: String): String = {
    JavaNetCodec.decode(s)
  }

  //  private def chars(s:String):Iterator[String] = new Iterator[String]{
  //
  //    var nextPos = 0
  //
  //    val maxPos = s.length - 1
  //
  //    override def hasNext: Boolean = nextPos <= maxPos
  //
  //    override def next(): String = {
  //      if(s.charAt(nextPos) == '%') {
  //        val res = s.substring(nextPos, nextPos + 3)
  //        nextPos += 3
  //        res
  //      }
  //      else {
  //        val res = s.substring(nextPos, nextPos + 1)
  //        nextPos += 1
  //        res
  //      }
  //    }
  //  }

  def quoteAuthority(a: Authority): String = {
    a.userInfo.fold("")(ui => Encoder.quote(ui.stringValue, Encoder.userInfoChars -- Set(':')) + "@") +
      Encoder.quote(a.host.stringValue, Encoder.registeredNameChars) +
      a.port.fold("")(":" + _.stringValue)
    //    new java.net.URI("s",a.userInfo.map(_.stringValue).orNull,a.host.stringValue,a.port.map(_.intValue).getOrElse(-1),null,null,null).getRawAuthority
  }

  def quoteSegment(s: Segment): String = {
    Encoder.quote(s.stringValue, Encoder.pathSegmentChars)
    //    new java.net.URI("s",s.stringValue,null).getRawSchemeSpecificPart
  }

  def quoteUri(uri: Uri): String = {
    val sScheme = uri.scheme.stringValue
    val sAuthority = uri.authority.map(a => "//" + a.stringValue).getOrElse("")
    val sPath = uri.path.stringValue
    val sQuery = uri.query.map(q => "?" + Encoder.quote(q.stringValue, Encoder.queryChars)).getOrElse("")
    val sFragment = uri.fragment.map(f => "#" + f.stringValue).getOrElse("")
    s"$sScheme:$sAuthority$sPath$sQuery$sFragment"
    //    new java.net.URI(
    //      uri.scheme.stringValue,
    //      uri.authority.map(encodeAuthority).orNull,
    //      uri.path.map(_.stringValue).getOrElse(""),
    //      uri.query.map(_.stringValue).orNull,
    //      uri.fragment.map(_.stringValue).orNull)
    //      .toString
  }

  def normalize(s: String): String = Normalizer.normalize(s, Normalizer.Form.NFC)

}
