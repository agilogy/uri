package com.agilogy.uri

import scala.util.{Failure, Success, Try}

sealed trait UriReference

trait Uri extends UriReference {

  def scheme: Scheme

  def authority: Option[Authority]

  def path: Path

  def query: Option[Query]

  def fragment: Option[Fragment]

  def stringValue:String = Encoder.quoteUri(this)

  def asciiStringValue:String = Encoder.asciiEncode(stringValue)

  def toJava:java.net.URI = {
//    def js = scheme.stringValue
//    val jui = authority.flatMap(_.userInfo).map(_.stringValue).orNull
//    val jh = authority.map(_.host.asciiStringValue).orNull
//    val jp = authority.flatMap(_.port).map(_.intValue).getOrElse(-1)
//    val jpath = path.map(_.stringValue).orNull
//    val jq = query.map(_.stringValue).orNull
//    val jf = fragment.map(_.stringValue).orNull
//    path match {
//      case Some(p) if !p.isAbsolute && !p.stringValue.isEmpty =>
//        val schemeSpecificPart = path.map(_.stringValue).getOrElse("") + query.map(q => "?" + q.stringValue).getOrElse("")
//        new java.net.URI(js, schemeSpecificPart, jf)
//      case _ => new java.net.URI(js, jui, jh, jp, jpath, jq, jf)
//    }
    new java.net.URI(stringValue)
  }

  override def equals(obj: Any): Boolean = obj match {
    case u: Uri => this.stringValue == u.stringValue
    case _ => false
  }
}

trait NoAuthorityUri extends Uri {
  override val authority: None.type = None
}

trait AuthorityUri extends Uri {
  def theAuthority: Authority

  override def authority: Some[Authority] = Some(theAuthority)
}

trait NoQueryUri extends Uri{
  override def query: None.type = None
}

trait QueryUri extends Uri{
  def theQuery:Query
  override def query: Some[Query] = Some(theQuery)
}

trait NoFragmentUri extends Uri{
  override def fragment: None.type = None
}

trait FragmentUri extends Uri{
  def theFragment:Fragment

  override def fragment: Some[Fragment] = Some(theFragment)
}


//@deprecated
//trait PathOnlyUri extends Uri {
//
//  def /(segment: Segment): PathOnlyUri = {
//    new CompleteUri(scheme, authority, Some(path.fold[Path](Path / segment)(_ / segment)), None, None) with PathOnlyUri
//        this.copy(path = Some(path.fold[Path](Path / segment)(_ / segment)))
//  }
//
//  def /(segment: String): PathOnlyUri = this / Segment(segment)
//
//  def ?(query: Query): UriWithQuery = UriWithQuery(scheme, authority, path, query)
//
//  def ?(query: String): UriWithQuery = this ? Query(query)
//
//  def ##(fragment: String): CompleteUri = this ## Fragment(fragment)
//
//  def ##(f: Fragment): CompleteUri = CompleteUri(scheme, authority, path, None, Some(f))
//
//    override def query: None.type = None
//
//    override def fragment: None.type = None
//
//}

trait UriBuilderQF[S<:Uri] extends NoQueryUri with NoFragmentUri {

  def ?(query: Query): S with QueryUri with UriBuilderF[S with QueryUri]
  def ?(query: String): S with QueryUri with UriBuilderF[S with QueryUri] = this ? Query(query)
  def ##(f: Fragment): S with NoQueryUri  with FragmentUri
  def ##(f: String): S with NoQueryUri  with FragmentUri = this ## Fragment(f)

}

trait UriBuilderF[S<:Uri] extends NoFragmentUri {

  def ##(f: Fragment): S with FragmentUri
  def ##(f: String): S with FragmentUri = this ## Fragment(f)

}


//case class UriWithQuery(scheme: Scheme, authority: Option[Authority], path: Option[Path], definedQuery: Query) extends Uri {
//  require(authority.nonEmpty || !path.exists(_.stringValue.startsWith("//")), s"An uri with no authority can't have a path starting with //: $this")
//
//  override def query: Some[Query] = Some(definedQuery)
//
//  override def fragment: None.type = None
//
//  def ##(fragment: String): CompleteUri = this ## Fragment(fragment)
//
//  def ##(f: Fragment): CompleteUri = CompleteUri(scheme, authority, path, Some(definedQuery), Some(f))
//
//}

case class CompleteUri(scheme: Scheme, authority: Option[Authority], path: Path, query: Option[Query] = None, fragment: Option[Fragment] = None) extends Uri {
  require(authority.nonEmpty || !path.stringValue.startsWith("//"), s"An uri with no authority can't have a path starting with //: $this")
  require(authority.isDefined || !path.isEmpty || query.isDefined, s"sExpected scheme-specific part with scheme ${scheme.stringValue}")
}


case class NoAuthorityPathUri(scheme: Scheme, path:Path) extends NoAuthorityUri with UriBuilderQF[NoAuthorityUri] {

  self =>

  def ?(q: Query): NoAuthorityUri with QueryUri with UriBuilderF[NoAuthorityUri with QueryUri] =
    new NoAuthorityUri with QueryUri with UriBuilderF[NoAuthorityUri with QueryUri]{

    override def scheme: Scheme = self.scheme

      override def path: Path = self.path

      override def theQuery: Query = q

    override def ##(f: Fragment): NoAuthorityUri with QueryUri with FragmentUri = new NoAuthorityUri with QueryUri with FragmentUri{
      override def scheme: Scheme = self.scheme

      override def path: Path = self.path

      override def theQuery: Query = q

      override def theFragment: Fragment = f
    }
  }

  override def ##(f: Fragment): NoAuthorityUri with NoQueryUri with FragmentUri = new NoAuthorityUri with NoQueryUri with FragmentUri{
    override def scheme: Scheme = self.scheme

    override def path: Path = self.path

    override def theFragment: Fragment = f

  }

}

case class AuthorityPathUri(scheme:Scheme, theAuthority: Authority, path:PathAbEmpty = Path.empty) extends AuthorityUri with UriBuilderQF[AuthorityUri] {
  self =>

  def /(s:Segment):AuthorityPathUri = this.copy(path = path / s)
  def /(s:String):AuthorityPathUri = this / Segment(s)

  override def ?(q: Query): AuthorityUri with QueryUri with UriBuilderF[AuthorityUri with QueryUri] =
    new AuthorityUri with QueryUri with UriBuilderF[AuthorityUri with QueryUri] {
    override def scheme: Scheme = self.scheme
    override def theAuthority: Authority = self.theAuthority
    override def path: PathAbEmpty = self.path
    override def theQuery: Query = q
    override def ##(f: Fragment): AuthorityUri with QueryUri with FragmentUri = new AuthorityUri with QueryUri with FragmentUri {
      override def scheme: Scheme = self.scheme
      override def theAuthority: Authority = self.theAuthority
      override def path: PathAbEmpty = self.path
      override def theQuery: Query = q
      override def theFragment: Fragment = f
    }

  }

  override def ##(f: Fragment): AuthorityUri with NoQueryUri with FragmentUri = new AuthorityUri with NoQueryUri with FragmentUri {
    override def scheme: Scheme = self.scheme
    override def theAuthority: Authority = self.theAuthority
    override def path: PathAbEmpty = self.path
    override def theFragment: Fragment = f
  }
}

object Uri {

  def apply(s: Scheme, p: Path): NoAuthorityPathUri = NoAuthorityPathUri(s,p)

  def apply(scheme: String, path: Path): NoAuthorityPathUri = apply(Scheme(scheme), path)

  def apply(s: Scheme, authority:Authority): AuthorityPathUri = AuthorityPathUri(s,authority)

  def apply(s: Scheme, host: RegisteredName): AuthorityPathUri = Uri(s, Authority(host))

  def apply(scheme: String, host: String):  AuthorityPathUri = Uri(Scheme(scheme), RegisteredName(host))

  def apply(s: Scheme, host: RegisteredName, port: Port):AuthorityPathUri = Uri(s,Authority(host, port))

  def apply(scheme: String, host: String, port: Int): AuthorityPathUri = Uri(Scheme(scheme), RegisteredName(host), Port(port))

  private val UriRe = "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?".r

  def parse(s:String):Try[Uri] = {
    def swap[T](optTry: Option[Try[T]]): Try[Option[T]] = {
      optTry.map(_.map(Some.apply)).getOrElse(Success(None))
    }

    s match {
      case UriRe(_,sScheme,_,sAuthority,sPath,_,sQuery,_,sFragment) =>
        for {
          scheme <- Success(Option(sScheme).map(s => Scheme(Encoder.decode(s))))
          authority <- swap(Option(sAuthority).map(Authority.parse))
          //swap(Option(sPath).map(Path.parse)).map(_.flatten)
          path <- Option(sPath).fold[Try[Path]](Success(Path.empty))(Path.parse)
          query <- Success(Option(sQuery).map(q => Query(Encoder.decode(q))))
          fragment <- Success(Option(sFragment).map(f => Fragment(Encoder.decode(f))))
        } yield CompleteUri(scheme.get,authority,path,query,fragment)
      case _ =>
        Failure(new IllegalArgumentException())
    }

  }

}