package com.agilogy.uri

import com.agilogy.uri.Path.AbsoluteOrEmpty

trait StandardRelativeReference extends UriReference {
  val authority:Option[Authority]
  val path:Path
  val query:Option[Query]
  val fragment:Option[Fragment]
}

case class RelativeReferenceWithAuthority(definedAuthority:Authority, path:Path with AbsoluteOrEmpty, query:Option[Query], fragment:Option[Fragment])
  extends StandardRelativeReference{
  override val authority: Some[Authority] = Some(definedAuthority)
}

case class RelativeReference(path:Path, query:Option[Query], fragment:Option[Fragment])
  extends StandardRelativeReference {
  override val authority: None.type = None
}
