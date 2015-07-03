package com.agilogy.uri

object Encoder {

  private[uri] val genDelims = ":/?#[]@"
  private[uri] val userInfo2Encode = "/?#[]@"
  private[uri] val path2Encode = "/?#[]"
  private[uri] val query2Encode = "#[]"
  private[uri] val fragment2Encode = query2Encode

  implicit class CharOps(c:Char){
    def pctEncoded:String = "%" + Integer.toHexString(c.toInt).toUpperCase
  }
  
  implicit class StringOps(s:String){
    
    def pctDecoded: String =  {
      val sb = new StringBuilder
      chars(s).foreach(c => sb.append(pctDecodeChar(c)))
      sb.toString()
    }

    def pctEncode(charactersToEncode:String):String = {
      val thisCode = charactersToEncode.map(c => c -> c.pctEncoded).toMap.withDefault(_.toString)
      val sb = new StringBuilder
      s.foreach(c => sb.append(thisCode(c)))
      sb.toString()
    }
  }

  private def pctDecodeChar(c:String):Char = {
    if (c.length == 1){
      c.charAt(0)
    } else if (c.startsWith("%")) {
      Integer.parseInt(c.substring(1), 16).toChar
    } else {
      throw new IllegalArgumentException(s"Can't decode $c")
    }
  }

  def encode(s: String, charactersToEncode: Set[Char]): String = s.pctEncode(charactersToEncode.mkString(""))

  def decode(s: String): String = s.pctDecoded


  private[uri] def chars(s:String):Iterator[String] = new Iterator[String]{

    var nextPos = 0

    val maxPos = s.length - 1

    override def hasNext: Boolean = nextPos <= maxPos

    override def next(): String = {
      if(s.charAt(nextPos) == '%') {
        val res = s.substring(nextPos, nextPos + 3)
        nextPos += 3
        res
      }
      else {
        val res = s.substring(nextPos, nextPos + 1)
        nextPos += 1
        res
      }
    }
  }
}
