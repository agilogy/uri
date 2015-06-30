package com.agilogy.uri

import com.agilogy.uri.Path.AbsoluteOrEmpty

trait UriReference

case class Scheme(value:String) extends AnyVal

case class Query(value:String) extends AnyVal

case class Fragment(value:String) extends AnyVal

trait Uri extends UriReference {
  val scheme:Scheme
  val `authority?`:Option[Authority]
  val path:Path
  val query:Option[Query]
  val fragment:Option[Fragment]
}

case class AbsoluteUri(scheme:Scheme, authority:Authority, path:Path with AbsoluteOrEmpty, query:Option[Query], fragment:Option[Fragment])
extends Uri {
  override val `authority?`: Some[Authority] = Some(authority)
}

case class UriWithoutAuthority(scheme:Scheme, override val path:Path, query:Option[Query], fragment:Option[Fragment])
extends Uri {
  override val `authority?`: None.type = None
}
