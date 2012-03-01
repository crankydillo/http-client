package org.beeherd.client.http

import org.beeherd.client._

class HttpRequest(
    val host: String
    , val path: String = "/"
    , val method: RequestMethod.Value = RequestMethod.Get
    , val headers: Map[String, List[String]] = Map()
    , val content: Option[Content] = None
    , val params: Map[String, String] = Map()
    , val protocol: String = "http"
    , val port: Int = 80
  ) extends Request {
  lazy val url = protocol + "://" + host + ":" + port + path
}

object HttpRequest {

  lazy val UrlRegex = """(([A-Za-z]+)://)?(\w+(\.\w+)*)(:(\d+))?(.*)""".r

  def unapply(request: HttpRequest): Option[(List[String], RequestMethod.Value)] = {
    if (request == null)
      return None
    val method = request.method
    if (method == null)
      return None
    val path = request.path
    if (path == null)
      return Some((Nil, method))
    if (path.length == 0) {
      return Some((Nil, method))
    }
    val tmp = if (path.startsWith("/")) path.drop(1) else path
    Some((tmp.split("/").toList, method))
  }

  /**
  * Parse a url into (protocol, host, port, path)
  *
  * @param url
  */
  def parseUrl(url: String): (String, String, Int, String) = {
    try {
      val UrlRegex(_, protocol, host, _, _, port, path) = url
      (
        if (protocol == null) "http" else protocol
        , host
        , if (port == null) 80 else port.toInt
        , if (path == null) "" else path
      )
    } catch {
      case e:MatchError => throw new IllegalArgumentException(url + 
        " cannot be parsed as a URL.")
    }
  }
}

object RequestMethod extends Enumeration {
  val Get, Post, Put, Delete = Value

  def create(str: String): RequestMethod.Value = {
    if (str == null || str.trim.length == 0)
      throw new IllegalArgumentException("The request method must not be null or the empty string.");
    str.toLowerCase match {
      case "get" => Get
      case "post" => Post
      case "put" => Put
      case "delete" => Delete
      case "del" => Delete
      case _ => throw new IllegalArgumentException(str + " is not supported.")
    }
  }
}
