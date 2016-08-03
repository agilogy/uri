package com.agilogy.uri

import scala.util.{Success, Try}

sealed trait PathType
object PathType{
  case object Absolute extends PathType
  case object Rootless extends PathType
  case object Empty extends PathType
}


sealed trait Path {
  type PathWithSegmentsType <: Path with PathWithSegments
  def pathType:PathType
  def isAbsolute: Boolean = pathType == PathType.Absolute
  def isRootless: Boolean = pathType == PathType.Rootless
  def isEmpty: Boolean = pathType == PathType.Empty
  def segments:Seq[Segment]
  def stringValue:String
  def asciiStringValue:String = Encoder.asciiEncode(stringValue)
//  override def toString: String = segments.map(_.toString).mkString("/")
  def /(s:Segment):PathWithSegmentsType
  def /(s:String): PathWithSegmentsType = this / Segment(s)
}

object Path{
  val empty = EmptyPath
  val Slash = Path / ""
  def / (s:Segment): AbsoluteSingleSegmentPath = AbsoluteSingleSegmentPath(s)
  def / (s:String): AbsoluteSingleSegmentPath = Path / Segment(s)
  def apply(s:Segment): RootlessPath = RootlessSingleSegmentPath(s)
  def apply(s:String): RootlessPath = apply(Segment(s))

  def absoluteOrEmpty(s:Segment *): PathAbEmpty = {
    if(s.isEmpty) empty
    else absolute(s :_*)
  }
  def absolute(s:Segment *): AbsolutePath = AbsolutePath(s :_*)
  def rootless(s:Segment *): RootlessPath = RootlessPath(s :_*)

  private val AbsolutePathRe = "(/[^?#]*)+".r
  private val RelativePathRe = "([^?#/]+)(/[^?#]*)*".r

  private val SegmentRe = "/([^?#/]*)".r

  def parse(s:String):Try[Path] = {
    s match {
      case "" => Success(Path.empty)
      case AbsolutePathRe(_) =>
        val res = SegmentRe.findAllIn(s).matchData.foldLeft[PathAbEmpty](Path.empty){
          case (acc,m) => acc / Encoder.decode(m.group(1))
        }
        Success(res)
      case RelativePathRe(headSegment,segmentsOrNull) =>
        val segmentsString = Option(segmentsOrNull).getOrElse("")
        val tailSegments = SegmentRe.findAllIn(segmentsString).matchData.map(m => m.group(1))
        val segments = (headSegment +: tailSegments.toSeq).map(s => Segment(Encoder.decode(s)))
        Success(RootlessPath(segments.toSeq :_*))
    }
  }

}

trait PathAbEmpty extends Path{
  type PathWithSegmentsType <: PathAbEmpty with PathWithSegments
}

trait PathWithSegments {
  self:Path =>
}

trait AbsolutePath extends PathWithSegments with PathAbEmpty{
  type PathWithSegmentsType = NonEmptyAbsolutePath
//  def /(s:String): NonEmptyAbsolutePath = this / Segment(s)


  override def pathType: PathType = PathType.Absolute

  def /(s:Segment): NonEmptyAbsolutePath = NonEmptyAbsolutePath(this,s)
  override def stringValue: String = segments.map(s => "/" + Encoder.quoteSegment(s)).mkString("")

  override def toString: String = s"""AbsolutePath(${segments.map(_.toString).mkString(",")})"""
}

object AbsolutePath{
  def apply(segments:Segment*):AbsolutePath = {
    require(segments.nonEmpty)
    val (head :: tail) = segments.toList
    tail.foldLeft[AbsolutePath](AbsoluteSingleSegmentPath(head))(_ / _)
  }
}

case class NonEmptyAbsolutePath(parent:AbsolutePath, segment:Segment) extends AbsolutePath with PathWithSegments{
  override def segments: Seq[Segment] = parent.segments :+ segment
}

trait RootlessPath extends Path{

  type PathWithSegmentsType = ConsRootlessPath


  override def pathType: PathType = PathType.Rootless

  def parentOption:Option[RootlessPath]
  val segment:Segment

  override def stringValue: String = segments.map(s => Encoder.quoteSegment(s)).mkString("/")
  override def toString: String = s"""RootlessPath(${segments.map(_.toString).mkString(",")})"""

}

object RootlessPath{
  def apply(segments:Segment*):RootlessPath = {
    require(segments.nonEmpty)
    val (head :: tail) = segments.toList
    tail.foldLeft[RootlessPath](RootlessSingleSegmentPath(head))(_ / _)
  }
}

case class ConsRootlessPath(parent:RootlessPath, segment:Segment) extends RootlessPath with PathWithSegments{

  def /(s:Segment): ConsRootlessPath = ConsRootlessPath(this,s)

  override def segments: Seq[Segment] = parent.segments :+ segment

  override def parentOption: Some[RootlessPath] = Some(parent)
}

case class AbsoluteSingleSegmentPath(segment: Segment) extends AbsolutePath with PathWithSegments {

  override def segments: Seq[Segment] = Seq(segment)
}

case class RootlessSingleSegmentPath(segment: Segment) extends RootlessPath with PathWithSegments{

  require(segment.stringValue.nonEmpty, "The first segment of a rootless path can't be empty")

  override def parentOption: None.type = None

  override def segments: Seq[Segment] = Seq(segment)

  override def /(s: Segment): ConsRootlessPath = ConsRootlessPath(this,s)
}

case object EmptyPath extends PathAbEmpty {

  type PathWithSegmentsType = AbsoluteSingleSegmentPath

  override def pathType: PathType = PathType.Empty

  override def segments: Seq[Segment] = Seq.empty

  override def stringValue: String = ""

  //  override def toString: String = segments.map(_.toString).mkString("/")
  override def /(s: Segment): AbsoluteSingleSegmentPath = AbsoluteSingleSegmentPath(s)
}