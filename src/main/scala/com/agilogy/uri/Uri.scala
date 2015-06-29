package com.agilogy.uri

import com.agilogy.uri.Path.SlashOrEmpty

trait UriReference

case class Scheme(value:String) extends AnyVal

case class UserInfo(value:String) extends AnyVal

case class Host(value:String) extends AnyVal

case class Port(value:Int) extends AnyVal

case class Authority(userInfo:Option[UserInfo], host:Host, port:Option[Port])

case class Query(value:String) extends AnyVal

case class Fragment(value:String) extends AnyVal

trait HasQuery{
  val query:Option[Query]
}

trait HasFragment{
  val fragment:Option[Fragment]
}

trait Uri extends UriReference with HasQuery with HasFragment{
  val scheme:Scheme
  val authority:Option[Authority]
  val path:Path
}

case class UriWithAuthority(scheme:Scheme, definedAuthority:Authority, override val path:Path with SlashOrEmpty, query:Option[Query], fragment:Option[Fragment])
extends Uri {
  override val authority: Some[Authority] = Some(definedAuthority)
}

case class UriWithoutAuthority(scheme:Scheme, override val path:Path, query:Option[Query], fragment:Option[Fragment])
extends Uri {
  override val authority: None.type = None
}

trait RelativeReference extends UriReference with HasQuery with HasFragment{
  val authority:Option[Authority]
  val path:Path
}

case class RelativeReferenceWithAuthority(definedAuthority:Authority, path:Path with SlashOrEmpty, query:Option[Query], fragment:Option[Fragment])
extends RelativeReference{
  override val authority: Some[Authority] = Some(definedAuthority)
}

case class RelativeReferenceWithoutAuthority(path:Path, query:Option[Query], fragment:Option[Fragment])
extends RelativeReference {
  override val authority: None.type = None
}