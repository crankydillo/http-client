/*
 * Request.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package org.beeherd.dispatcher

import java.io.StringReader;
import java.util.{Map => JMap, List => JList}


object JCollections {
   import scala.collection.JavaConversions._

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
  def headers: Map[String, List[String]]
  def content: Option[Content]
}

class DefaultRequest(
    val headers: Map[String, List[String]] = Map()
    , val content: Option[Content] = None
  ) extends Request {

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


