package com.agilogy.uri

import com.agilogy.uri.Path.AbsoluteOrEmpty

trait UriReference{
  val `scheme?`:Option[Scheme]
  val `authority?`: Option[Authority]
  val path: Path
  val query: Option[Query]
  val fragment: Option[Fragment]

  lazy val encoded = {

    val result = new StringBuffer
    `scheme?`.foreach { scheme =>
      result.append(scheme.toString)
      result.append(":")
    }
    `authority?`.foreach { authority =>
      result.append("//")
      result.append(authority.toString)
    }
    result.append(path)
    query.foreach{ q =>
      result.append("?")
      result.append(q)
    }
    fragment.foreach{ f =>
      result.append("#")
      result.append(f)
    }
    result.toString

  }

  override def toString: String = encoded

}

case class Scheme(value: String) extends AnyVal{
  override def toString: String = value
}

object Scheme {
  val http = Scheme("http")
  val https = Scheme("https")
}

case class Query(value: String) extends AnyVal{
  override def toString: String = value
}

case class Fragment(value: String) extends AnyVal{
  override def toString: String = value
}

trait Uri extends UriReference {
  val scheme: Scheme
  val `scheme?`:Some[Scheme] = Some(scheme)
}

object Uri {

  def absolute(scheme: String, authority: Authority, path: Path with AbsoluteOrEmpty, query: Option[String] = None, fragment: Option[String] = None): AbsoluteUri =
    AbsoluteUri(Scheme(scheme), authority, path, query.map(Query.apply), fragment.map(Fragment.apply))
}

case class AbsoluteUri(scheme: Scheme, authority: Authority, path: Path with AbsoluteOrEmpty, query: Option[Query], fragment: Option[Fragment])
  extends Uri {
  override val `authority?`: Some[Authority] = Some(authority)

}

case class UriWithoutAuthority(scheme: Scheme, override val path: Path, query: Option[Query], fragment: Option[Fragment])
  extends Uri {
  override val `authority?`: None.type = None
}
