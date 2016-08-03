package com.agilogy.uri

import scala.util.{Success, Try}

sealed trait Path {
  type PathWithSegmentsType <: Path with PathWithSegments
  def isAbsolute: Boolean
  def segments:Seq[Segment]
  def stringValue:String
  def asciiStringValue:String = Encoder.asciiEncode(stringValue)
//  override def toString: String = segments.map(_.toString).mkString("/")
  def /(s:Segment):PathWithSegmentsType
  def /(s:String): PathWithSegmentsType = this / Segment(s)
}

object Path{
  val Slash = Path / ""
  def / (s:Segment): AbsoluteSingleSegmentPath = AbsoluteSingleSegmentPath(s)
  def / (s:String): AbsoluteSingleSegmentPath = Path / Segment(s)
  def apply(s:Segment): RootlessPath = RootlessSingleSegmentPath(s)
  def apply(s:String): RootlessPath = apply(Segment(s))

  private val AbsolutePathRe = "(/[^?#]*)+".r
  private val RelativePathRe = "([^?#/]+)(/[^?#]*)*".r

  private val SegmentRe = "/([^?#/]*)".r

  def parse(s:String):Try[Option[Path]] = {
    s match {
      case "" => Success(None)
      case AbsolutePathRe(_) =>
        val res = SegmentRe.findAllIn(s).matchData.foldLeft[Option[AbsolutePath]](None){
          case (acc,m) => Some(acc.fold[AbsolutePath](Path / Encoder.decode(m.group(1)))(_ / Encoder.decode(m.group(1))))
        }
        Success(res)
      case RelativePathRe(headSegment,segmentsOrNull) =>
//        println(s"Matched relative $headSegment, $segmentsOrNull")
        val segments = Option(segmentsOrNull).getOrElse("")
        val res = SegmentRe.findAllIn(segments).matchData.foldLeft[RootlessPath](RootlessSingleSegmentPath(Segment(Encoder.decode(headSegment)))){
          case (acc,m) => acc / Encoder.decode(m.group(1))
        }
        Success(Some(res))
    }
  }

}

trait PathWithSegments {
  self:Path =>
}

trait AbsolutePath extends Path{
  type PathWithSegmentsType = NonEmptyAbsolutePath
//  def /(s:String): NonEmptyAbsolutePath = this / Segment(s)
  def /(s:Segment): NonEmptyAbsolutePath = NonEmptyAbsolutePath(this,s)
  override def isAbsolute: Boolean = true
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

  def parentOption:Option[RootlessPath]
  val segment:Segment
  override def isAbsolute: Boolean = false

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

case class AbsoluteSingleSegmentPath(segment: Segment) extends AbsolutePath {

  override def segments: Seq[Segment] = Seq(segment)
}

case class RootlessSingleSegmentPath(segment: Segment) extends RootlessPath {

  require(segment.stringValue.nonEmpty, "The first segment of a rootless path can't be empty")

  override def parentOption: None.type = None

  override def segments: Seq[Segment] = Seq(segment)

  override def /(s: Segment): ConsRootlessPath = ConsRootlessPath(this,s)
}