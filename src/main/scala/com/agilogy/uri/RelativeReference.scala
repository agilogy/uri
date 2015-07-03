package com.agilogy.uri

import com.agilogy.uri.Path.AbsoluteOrEmpty

trait StandardRelativeReference extends UriReference{
  val `scheme?`: None.type = None
}

case class RelativeReferenceWithAuthority(authority:Authority, path:Path with AbsoluteOrEmpty, query:Option[Query], fragment:Option[Fragment])
  extends StandardRelativeReference{
  override val `authority?`: Some[Authority] = Some(authority)
}

case class RelativeReference(path:Path, query:Option[Query], fragment:Option[Fragment])
  extends StandardRelativeReference {
  override val `authority?`: None.type = None
}
