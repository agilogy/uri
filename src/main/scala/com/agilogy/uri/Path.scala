package com.agilogy.uri

sealed trait PathType
object PathType {
  case object Absolute extends PathType
  case object Rootless extends PathType
  case object Empty extends PathType
}

/**
  * @see <a href="https://tools.ietf.org/html/rfc3986#section-3.3">rfc3986#section-3.3</a>
  */
sealed trait Path {
  type PathWithSegmentsType <: Path with PathWithSegments
  def pathType: PathType
  def isAbsolute: Boolean = pathType == PathType.Absolute
  def isRootless: Boolean = pathType == PathType.Rootless
  def isEmpty: Boolean = pathType == PathType.Empty
  def segments: Seq[Segment]
  def stringValue: String
  def asciiStringValue: String = Encoder.asciiEncode(stringValue)
  //  override def toString: String = segments.map(_.toString).mkString("/")
  def /(s: Segment): PathWithSegmentsType
  def /(s: String): PathWithSegmentsType = this / Segment(s)
}

object Path {
  val empty: EmptyPath.type = EmptyPath
  val Slash: AbsoluteSingleSegmentPath = Path / ""
  def /(s: Segment): AbsoluteSingleSegmentPath = AbsoluteSingleSegmentPath(s)
  def /(s: String): AbsoluteSingleSegmentPath = Path / Segment(s)
  def apply(s: NonEmptySegment) = RootlessSingleSegmentPath(s)
  def apply(s: String): Either[FirstSegmentIsEmptyInRootlessPath.type, RootlessSingleSegmentPath] = Segment(s) match {
    case EmptySegment => Left(FirstSegmentIsEmptyInRootlessPath)
    case s:NonEmptySegment => Right(RootlessSingleSegmentPath(s))
  }

  def absoluteOrEmpty(s: Segment*): PathAbEmpty = {
    s match {
      case Seq()     => empty
      case h +: tail => absolute(h, tail: _*)
    }
  }
  def absolute(segment: Segment, moreSegments: Segment*): AbsolutePath = AbsolutePath(segment, moreSegments: _*)
  def rootless(segment: NonEmptySegment, moreSegments: Segment*): RootlessPath = RootlessPath(segment, moreSegments: _*)

  private val AbsolutePathRe = "(/[^?#]*)+".r
  private val RelativePathRe = "([^?#/]+)(/[^?#]*)*".r

  private val SegmentRe = "/([^?#/]*)".r

  def parse(s: String): Path = {
    s match {
      case "" => Path.empty
      case AbsolutePathRe(_) =>
        val res = SegmentRe.findAllIn(s).matchData.foldLeft[PathAbEmpty](Path.empty) {
          case (acc, m) => acc / Encoder.decode(m.group(1))
        }
        res
      case RelativePathRe(headSegment, segmentsOrNull) =>
        val segmentsString = Option(segmentsOrNull).getOrElse("")
        val tailSegments = SegmentRe.findAllIn(segmentsString).matchData.map(m => m.group(1))
        val h = NonEmptySegment(Encoder.decode(headSegment))
        val t = tailSegments.toList.map(s => Segment(Encoder.decode(s)))
        Path.rootless(h, t: _*)
    }
  }

}

/**
  * A path that begins with "/" or is empty
  * @see <a href="https://tools.ietf.org/html/rfc3986#section-3.3">rfc3986#section-3.3</a>
  */
trait PathAbEmpty extends Path {
  type PathWithSegmentsType <: PathAbEmpty with PathWithSegments
}

trait PathWithSegments {
  self: Path =>
}

/**
  * A path that begins with "/"
  * <p>
  * Note that it does NOT correspond to rfc3986's path-absolute, since it may begin with "//"
  * @see <a href="https://tools.ietf.org/html/rfc3986#section-3.3">rfc3986#section-3.3</a>
  */
trait AbsolutePath extends PathWithSegments with PathAbEmpty {
  type PathWithSegmentsType = NonEmptyAbsolutePath
  //  def /(s:String): NonEmptyAbsolutePath = this / Segment(s)

  override def pathType: PathType = PathType.Absolute

  def /(s: Segment): NonEmptyAbsolutePath = NonEmptyAbsolutePath(this, s)
  override def stringValue: String = segments.map(s => "/" + Encoder.quoteSegment(s)).mkString("")

  override def toString: String = s"""AbsolutePath(${segments.map(_.toString).mkString(",")})"""
}

object AbsolutePath {
  def apply(head: Segment, tail: Segment*): AbsolutePath = {
    tail.foldLeft[AbsolutePath](AbsoluteSingleSegmentPath(head))(_ / _)
  }
}

case class NonEmptyAbsolutePath(parent: AbsolutePath, segment: Segment) extends AbsolutePath with PathWithSegments {
  override def segments: Seq[Segment] = parent.segments :+ segment
}

/**
  * A path that begins with a non empty segment
  * @see <a href="https://tools.ietf.org/html/rfc3986#section-3.3">rfc3986#section-3.3</a>
  */
trait RootlessPath extends Path {

  type PathWithSegmentsType = ConsRootlessPath

  override def pathType: PathType = PathType.Rootless

  def parentOption: Option[RootlessPath]
  val segment: Segment

  override def stringValue: String = segments.map(s => Encoder.quoteSegment(s)).mkString("/")
  override def toString: String = s"""RootlessPath(${segments.map(_.toString).mkString(",")})"""

}

object RootlessPath {
  private[uri] def apply(head:NonEmptySegment, tail: Segment*): RootlessPath =
    tail.foldLeft[RootlessPath](RootlessSingleSegmentPath(head))(_ / _)
}

/**
  * @see <a href="https://tools.ietf.org/html/rfc3986#section-3.3">rfc3986#section-3.3</a>
  */
case class ConsRootlessPath(parent: RootlessPath, segment: Segment) extends RootlessPath with PathWithSegments {

  def /(s: Segment): ConsRootlessPath = ConsRootlessPath(this, s)

  override def segments: Seq[Segment] = parent.segments :+ segment

  override def parentOption: Some[RootlessPath] = Some(parent)
}

case class AbsoluteSingleSegmentPath(segment: Segment) extends AbsolutePath with PathWithSegments {

  override def segments: Seq[Segment] = Seq(segment)
}

/**
  * @see <a href="https://tools.ietf.org/html/rfc3986#section-3.3">rfc3986#section-3.3</a>
  */
case class RootlessSingleSegmentPath private[uri] (segment: NonEmptySegment) extends RootlessPath with PathWithSegments {

  override def parentOption: None.type = None

  override def segments: Seq[Segment] = Seq(segment)

  override def /(s: Segment): ConsRootlessPath = ConsRootlessPath(this, s)
}

/**
  * The path with zero characters
  * @see <a href="https://tools.ietf.org/html/rfc3986#section-3.3">rfc3986#section-3.3</a>
  */
case object EmptyPath extends PathAbEmpty {

  type PathWithSegmentsType = AbsoluteSingleSegmentPath

  override def pathType: PathType = PathType.Empty

  override def segments: Seq[Segment] = Seq.empty

  override def stringValue: String = ""

  //  override def toString: String = segments.map(_.toString).mkString("/")
  override def /(s: Segment): AbsoluteSingleSegmentPath = AbsoluteSingleSegmentPath(s)
}