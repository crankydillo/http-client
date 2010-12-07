package org.beeherd.dispatcher.http

import org.beeherd.dispatcher._

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
    Some((path.split("/").toList, method))
  }
}

object RequestMethod extends Enumeration {
  val Get, Post, Put, Delete = Value

  def create(str: String): RequestMethod.Value = {
    if (str == null || str.trim.length == 0)
      throw new IllegalArgumentException("The request method must not be null or the empty string.");
    if (str.equalsIgnoreCase("get"))
      return RequestMethod.Get;
    if (str.equalsIgnoreCase("post"))
      return RequestMethod.Post;
    if (str.equalsIgnoreCase("put"))
      return RequestMethod.Put;
    if (str.equalsIgnoreCase("delete"))
      return RequestMethod.Delete;
    throw new IllegalArgumentException(str + " is not supported.")
  }
}
