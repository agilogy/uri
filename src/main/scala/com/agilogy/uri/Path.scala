package com.agilogy.uri

sealed trait Path {

  def encoded:String

  val value: String

  override def toString: String = value

}

object / {

  def unapply(p:Path):Option[(Path, String)] = p match {
    case Path.AbsoluteCompound(a,s) => Some(a,s)
    case Path.RelativeCompound(r,s) => Some(r,s)
    case _ => None
  }

}


object Path {

  def apply(s: String): Path = s match {
    case "" => Empty
    case "/" => Path.Slash
    case _ if s.startsWith("/") =>
      val p = s.split("/").foldLeft[Absolute](Slash) {
        case (acc, segment) => acc / segment
      }
      if (s.endsWith("/")) p / ""
      else p
    case _ =>
      val p = s.split("/").foldLeft[Relative](Base) {
        case (acc, segment) => acc / segment
      }
      if (s.endsWith("/")) p / ""
      else p
  }

  trait AbsoluteOrEmpty {
    self: Path =>
  }

  case object Empty extends Path with AbsoluteOrEmpty {

    override val value: String = ""

    override lazy val encoded: String = ""
  }

  trait Absolute extends Path with AbsoluteOrEmpty {

    def /(segment: String): Absolute

  }

  object Slash extends Absolute {

    override val value: String = "/"

    override lazy val encoded: String = "/"

    override def /(segment: String): Absolute = {
      if (segment.isEmpty || segment == ".") Slash
      else new AbsoluteCompound(Slash, segment)
    }


  }

  case class AbsoluteCompound(parent: Absolute, segment: String) extends Absolute {

    override val value: String = {
      if(parent == Slash) s"/$segment"
      else s"${parent.value}/$segment"
    }

    override lazy val encoded: String = {
      import Encoder._
      val encodedSegment = segment.pctEncode(path2Encode)
      if(parent == Slash) s"/$encodedSegment"
      else s"${parent.encoded}/$encodedSegment"
    }

    def /(segment: String): Absolute = {
      if (this.segment.isEmpty) parent / segment
      else if (segment == "..") parent
      else if (segment == ".") this
      else new AbsoluteCompound(this, segment)
    }

  }

  trait Relative extends Path {

    def /(segment: String): Relative

  }

  case object Base extends Relative {

    override def /(segment: String): Relative = {
      if(segment == ".") Base
      else RelativeCompound(this, segment)
    }

    override val value: String = "."

    override def encoded: String = "."
  }

  case class RelativeCompound(parent: Relative, segment: String) extends Relative {

    override val value: String = {
      if (parent == Base) segment
      else s"${parent.value}/$segment"
    }

    override lazy val encoded: String = {
      import Encoder._
      val encodedSegment = segment.pctEncode(path2Encode)
      if (parent == Base) encodedSegment
      else s"${parent.encoded}/$encodedSegment"
    }


    override def /(segment: String): Relative = {
      if (this.segment.isEmpty) parent / segment
      else if (segment == ".") this
      else if (segment == ".." && this.segment != "..") parent
      else RelativeCompound(this, segment)

    }

  }

}

