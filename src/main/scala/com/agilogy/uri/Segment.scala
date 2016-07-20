package com.agilogy.uri

// TODO: Enforce segment constraints
abstract case class Segment private (stringValue:String){
  def asciiStringValue:String = Encoder.asciiEncode(stringValue)
  override def toString: String = s"""Segment("$stringValue")"""
}

object Segment{
  val Empty = new Segment(""){}

  def apply(stringValue:String):Segment = if(stringValue.isEmpty) Empty else new Segment(Encoder.normalize(stringValue)){}
}