/*
 * Request.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package org.beeherd.dispatcher

import java.io.StringReader;
import java.util.{Map => JMap, List => JList}

import scala.collection.JavaConversions.asMap

object JCollections {
   import scala.collection.JavaConversions.asBuffer

   def toList[T](jList: JList[T]):List[T] =
     toListOption(jList).getOrElse(List())

   def toListOption[T](jList: JList[T]):Option[List[T]] =
     Option(jList).map { asBuffer(_).toList }

   def toMap[K,V](jMap:JMap[K,V]):Map[K,V] =
     toMapOption(jMap).getOrElse(Map())

   def toMapOption[K,V](jMap:JMap[K,V]):Option[Map[K,V]] =
     Option(jMap) map { asMap(_).toMap }
}

trait Request {
  def path: Path
  def method: RequestMethod.Value
  def headers: Map[String, List[String]]
  def content: Option[Content]
  def params: Map[String, String]
}

object Request {
  def apply(path: Path, method: RequestMethod.Value,
      headers: JMap[String, JList[String]], content: String) = 
    new DefaultRequest(path, method, 
      asMap(headers).mapValues(JCollections.toList).toMap
      , None, Map())

  // This method is what allows us to do pattern matching on Request instances.
  // For example, this allows us to implement Dispatcher.dispatch(Request) as it
  // is.
  def unapply(request: Request): Option[(List[String], RequestMethod.Value)] = {
    if (request == null)
      return None
    val method = request.method
    if (method == null)
      return None
    val path = request.path
    if (path == null)
      return Some((Nil, method))
    val trimmed = path.path
    if (trimmed.length == 0) {
      return Some((Nil, method))
    }
    Some((trimmed.split("/").toList, method))
  }
}

class DefaultRequest(
    val path: Path
    , val method: RequestMethod.Value
    , val headers: Map[String, List[String]] = Map()
    , val content: Option[Content] = None
    , val params: Map[String, String] = Map()
  ) extends Request {

  def this(
    path: Path
    , method: RequestMethod.Value
    , content: Content
  ) = this(path, method, content = Some(content))


  def this(
      path: Path
      , method: RequestMethod.Value
      , params: JMap[String, String]
    ) = this(
      path, method, params = asMap(params).toMap
    )

  // Originally, the content was XML and the variable was xml.  Needless to say
  // there are quite a few places in the code where request.xml.  This is a
  // convience method to support that.
  val xml: Option[scala.xml.Node] =
    content match {
      case Some(XmlContent(xml)) => Some(xml)
       case _ => None
    }

  def headerValue(header: String): Option[List[String]] = headers.get(header)

  def headerValueIgnoreCase(header: String): Option[List[String]] = {
    if (header == null)
      return None
    headers.find {h => header.equalsIgnoreCase(h._1)} match {
      case Some(o) => Some(o._2)
      case None => None
    }
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

object Path {
  def unapply(path: String): Option[List[String]] = {
    if (path == null) {
      return None
    }
    val trimmed = path.trim
    if (trimmed.length == 0) {
      return None
    }
    Some(path.split("/").toList)
  }
}
class Path(val path: String)
