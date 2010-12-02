package org.beeherd.dispatcher.http

import org.beeherd.dispatcher._

class HttpRequest(
    val domain: String
    , val path: Path = new Path("/")
    , val method: RequestMethod.Value = RequestMethod.Get
    , val headers: Map[String, List[String]] = Map()
    , val content: Option[Content] = None
    , val params: Map[String, String] = Map()
    , val protocol: String = "http"
    , val port: Int = 80
  ) extends Request {
  lazy val url = protocol + "://" + domain + ":" + port + path.path
}

object HttpRequest {

  def unapply(request: HttpRequest): Option[(List[String], RequestMethod.Value)] = 
    Request.unapply(request)
}

