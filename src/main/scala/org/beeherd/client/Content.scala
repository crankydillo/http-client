/*
 * Content.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package org.beeherd.client

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, BufferedReader,
InputStream, InputStreamReader, StringReader}
import java.nio.charset.Charset

import org.apache.commons.io.IOUtils
import org.apache.log4j.Logger

abstract class Content(val ctype: String, val length: Long) {
  def createStream: InputStream
}

object StringContent {
  def apply(
      str: String
      , ctype: String
      , charsetName: String
    ) = new StringContent(str, ctype, Charset.forName(charsetName))
}

case class StringContent(
    str: String
    , override val ctype: String = "text/plain"
    , charset: Charset = Charset.defaultCharset
  ) extends Content(ctype, str.size) {

  override def createStream = new ByteArrayInputStream(str.getBytes(charset))

  override def toString = str;
}

case class XHtmlContent(val xml: scala.xml.Node) 
extends Content("text/html", xml.toString.getBytes("UTF-8").length) {

  override def createStream = {
    val bytes = xml.toString.getBytes("UTF-8");
    new ByteArrayInputStream(bytes);
  }

  override def toString = xml.toString
}

case class HtmlContent(val str: String) extends Content("text/html", str.getBytes("UTF-8").length
    ) {
  override def createStream = {
    val bytes = str.getBytes("UTF-8");
    new ByteArrayInputStream(bytes);
  }

  override def toString = str;
}

object XmlContent {
  def apply(string: String) = 
    new XmlContent(
      try {
        scala.xml.XML.loadString(string)
      } catch {
        case e:Exception => throw new IllegalArgumentException(e)
      }
    )
}

case class XmlContent(
  xml: scala.xml.Node
) extends Content("text/xml", xml.toString.getBytes("UTF-8").length) {

  override def createStream = {
    val bytes = xml.toString.getBytes("UTF-8");
    new ByteArrayInputStream(bytes);
  }

  override def toString = xml.toString
}

case class ByteArrayContent(val bytes: Array[Byte]) extends Content("application/octet-stream", bytes.length) {
  override def createStream = new ByteArrayInputStream(bytes)
}

case class ZipContent(val bytes: Array[Byte]) extends Content("application/zip", bytes.length) {
  override def createStream = new ByteArrayInputStream(bytes)
}

case class StreamedContent(
    val stream: InputStream
    , override val ctype: String
    , override val length: Long)
extends Content(ctype, length) {
  override def createStream = stream;
}

case class Part(
  val name: String 
  , val content: Content
  , val headers: List[(String, String)] = List()
)

case class MultiPartContent(
    parts: List[Part]
    , override val ctype: String = "multipart/form-data"
  ) extends 
Content(
  ctype
  , parts.foldLeft (0L) {case (sum, Part(_,_,c)) => sum + c.length}
) {

  override def createStream = throw new UnsupportedOperationException(
      "Cannot get a stream for multipart/form-data is not supported yet.");
}


object ContentCreator {
  val log = Logger.getLogger(classOf[ContentCreator]);
}

/**
* Creates Content.
*/
class ContentCreator {
  def create(in: InputStream, mimeType: String): Option[Content] = {
    try {
      val lowerCaseMimeType = mimeType.toLowerCase;
      ContentCreator.log.debug(mimeType);

      if (in == null || mimeType == null || mimeType.trim.length == 0) {
        None
      } else if ("application/zip" == lowerCaseMimeType) {
        createZipContent(in)
      } else if (lowerCaseMimeType.startsWith("text/xml") ||
          lowerCaseMimeType.startsWith("application/xml")) {
        val str = 
          createStringContent(in) match {
            case None => ""
            case Some(sc) => sc.str
          }
        createXmlContent(str);
      } else if (lowerCaseMimeType.startsWith("multipart/form-data")) {
        createMultiPartContent(in, mimeType);
      } else {
        if (in == null) None
        else createStringContent(in)
      }
    } catch {
      // Just treat it as plain text
      case e:Exception => {
        ContentCreator.log.warn(e.getMessage);
        None
      }
    }
  }

  def createXmlContent(stream: InputStream): Option[XmlContent] = {
    if (stream == null) {
      None
    } else {
      createStringContent(stream) match {
        case None => None
        case Some(sc) => createXmlContent(sc.str)
      }
    }
  }

  def createXmlContent(str: String): Option[XmlContent] = {
    try {
      if (str.trim.length == 0)
        return None;

      val xml = scala.xml.XML.load(new StringReader(str));
      Some(XmlContent(xml))
    } catch {
      case e:Exception => {
        ContentCreator.log.warn("Could not convert message body into XmlContent.  " + e.getMessage);
        None
      }   
    }   
  }

  private def createZipContent(istream: java.io.InputStream): Option[ZipContent] = { 
    ContentCreator.log.debug("I am attempting to create ZipContent");
    val out = new ByteArrayOutputStream;
    val arr = new Array[Byte](1024);
    var len = 1;
    while (len > 0) {
      len = istream.read(arr, 0, arr.length);
      ContentCreator.log.debug("Read " + len + " bytes from the InputStream.");
      if (len > 0)
        out.write(arr, 0, len);
    }   
    istream.close;
    Some(ZipContent(out.toByteArray))
  }

  private def createStringContent(istream: java.io.InputStream): Option[StringContent] = { 
    val reader = new BufferedReader(new InputStreamReader(istream));
    val out = new StringBuilder();
    var line: String = ""; 
    while (line != null) {
      line = reader.readLine;
      if (line != null) {
        out.append(line + "\n");
      }
    }   
    istream.close();
    Some(StringContent(out.toString(), "text/plain"));
  }   

  private def createMultiPartContent(in: InputStream, contentType: String): Option[MultiPartContent] = { 
    throw new UnsupportedOperationException("MultipartContent not supported yet.");
  }   
}
