package com.agilogy.uri

sealed trait Path {
  val value: String
  def /(rhs:Path):Path
}

object Path {

  def apply(s: String): Path = s match {
    case "" => Path.Empty
    case "/" => Path.Slash
    case _ if s.startsWith("/") =>
      val p = s.split("/").filter(_.nonEmpty).foldLeft[Path](Path.Empty){
        case (acc, segment) => acc / Path.Absolute(segment,Path.Empty)
      }
      if(s.endsWith("/")) p / Path.Empty
      else p
    case _ =>
      val firstSlash = s.indexOf("/")
      val firstSegment = if(firstSlash != -1) s.substring(0,firstSlash) else s
      val rest = s.substring(firstSlash)
      Relative(firstSegment, Path(rest).asInstanceOf[Path with SlashOrEmpty])
  }


  sealed trait SlashOrEmpty {

    val value: String

    def /(path: Path): Path with SlashOrEmpty = path match {
      case Empty => Slash
      case Relative(h, t) => Absolute(h, t)
      case a:Absolute => a
      case Slash => Slash
    }
  }

  case object Empty extends Path with SlashOrEmpty {

    override val value: String = ""
  }

  case object Slash extends Path with SlashOrEmpty {

    override val value: String = "/"
  }

  case class Absolute(head: String, tail: SlashOrEmpty) extends Path with SlashOrEmpty {
    override def /(path: Path):Absolute = Absolute(head, tail / path)

    override val value: String = s"/$head${tail.value}"
  }

  case class Relative(head: String, tail: SlashOrEmpty) extends Path {
    override val value: String = s"$head${tail.value}"

    override def /(rhs: Path): Path = Relative(head, tail / rhs)
  }

}


//trait AbsoluteOrEmpty {
//  self: Path =>
//}
//
//case object EmptyPath extends Path with AbsoluteOrEmpty {
//  override val absolute: Boolean = false
//  override val finalSlash: Boolean = false
//  override val segments: Seq[PathSegment] = Seq.empty
//}
//
//case class AbsolutePath(override val segments: Seq[PathSegment], finalSlash: Boolean) extends Path with AbsoluteOrEmpty {
//  override val absolute: Boolean = true
//}
//
//case object Root extends AbsolutePath(Seq.empty, finalSlash = false)
//
//case class RelativePath(segments: Seq[PathSegment], finalSlash: Boolean) extends Path {
//  override val absolute: Boolean = false
//}
//
//object Path {
//
//  val / = Root
//}
//
