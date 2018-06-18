package com.agilogy.uri

trait Segment{
  def stringValue:String
  def asciiStringValue: String = Encoder.asciiEncode(stringValue)
  override def toString: String = s"""Segment("$stringValue")"""
}

// TODO: Enforce segment constraints
abstract case class NonEmptySegment private (stringValue: String) extends Segment

object NonEmptySegment{
  private[uri] def apply(stringValue:String): NonEmptySegment = new NonEmptySegment(Encoder.normalize(stringValue)){}
}

case object EmptySegment extends Segment {
  override def stringValue: String = ""
}

object Segment {
  val Empty: EmptySegment.type = EmptySegment

  def apply(stringValue: String): Segment = if (stringValue.isEmpty) Empty else NonEmptySegment(stringValue)
}