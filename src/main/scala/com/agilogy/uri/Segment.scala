package com.agilogy.uri

sealed trait Segment extends UriPart {
  def stringValue: String
}

// TODO: Enforce segment constraints
sealed abstract case class NonEmptySegment private(stringValue: String) extends Segment

object NonEmptySegment {
  private[uri] def apply(stringValue: String): NonEmptySegment = new NonEmptySegment(Encoder.normalize(stringValue)) {}
}

case object EmptySegment extends Segment {
  override def stringValue: String = ""
}

object Segment {
  val Empty: EmptySegment.type = EmptySegment

  def apply(stringValue: String): Segment = if (stringValue.isEmpty) Empty else NonEmptySegment(stringValue)
}