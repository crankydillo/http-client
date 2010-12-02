/*
 * Content.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package org.beeherd.dispatcher

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, BufferedReader,
InputStream, InputStreamReader, StringReader}

import org.apache.commons.fileupload.MultipartStream
import org.apache.commons.io.IOUtils
import org.apache.log4j.Logger

abstract class Content(val ctype: String, val length: Long) {
  /**
  * Creates a <b>new</b> stream for each call to this method.
  */
  def createStream: InputStream
}

// TODO: Handle encodings properly...
case class StringContent(val str: String, contenttype: String) extends Content(contenttype, str.size) {
  override def createStream = new ByteArrayInputStream(str.getBytes)

  override def toString = str;
}

case class XHtmlContent(val xml: scala.xml.Node) extends Content("text/html", xml.toString.getBytes("UTF-8").length
    ) {
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

// TODO: Use this on the Response side.
case class XmlContent(val xml: scala.xml.Node) extends Content("text/xml", xml.toString.getBytes("UTF-8").length
    ) {
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
    val stream: InputStream, 
    override val ctype: String, 
    override val length: Long)
extends Content(ctype, length) {
  override def createStream = stream;
}

/**
 * Represent multipart/form-data content.
 * <p>
 * It has not been tested, but this will probably only work for sending one file.
 * <p>
 * This is a little different than the other cases.  Currently, it is only used
 * on the Request side of things.
 */
case class MultiPartContent(stream: InputStream, boundary: Array[Byte]) extends Content("multipart/form-data", -1) {

  // Consider replacing dependency on Apache Commons FileUpload with Apache Mime4j.

  // Don't know about using -1 as length...  I just know I don't want to read the stream to calculate length

  override def createStream = throw new UnsupportedOperationException("Cannot get a stream for multipart/form-data is not supported yet.");

  /**
   * The parts of the stream.
   * @return a list of headers (name, value) and byte array pairs representing
   * the parts.
   */
  lazy val parts: List[Part] = {
    // Should this stuff be split out?  The other cases don't take streams and convert...

    def parseHeaders(str: String): List[(String, String)] = {
      str.split("\n")
      .filter {s => s.indexOf(": ") != -1}
      .map {s => {
            val a = s.split(": ");
            assert(a.size == 2);
            (a(0).trim, a(1).trim)
          }
        }.toList
    }

    def isBinary(headers: List[(String, String)]): Boolean = headers.exists {
          h => h._1.equalsIgnoreCase("content-type") && h._2.toLowerCase.startsWith("application/octet-stream")
        }

    val multipartStream =  new MultipartStream(stream, boundary);
    var nextPart = multipartStream.skipPreamble;
    val parts = new scala.collection.mutable.ArrayBuffer[Part]();

    while(nextPart) {
      val headerStr = multipartStream.readHeaders;
      val headers = parseHeaders(headerStr);

      val data = new ByteArrayOutputStream();
      multipartStream.readBodyData(data);

      // TODO: Handle encoding
      val content = 
        if (isBinary(headers))
          ByteArrayContent(data.toByteArray);
        else
          StringContent(new String(data.toByteArray), "text/plain");

        // not sure where to get name on the read side
      val aPart = new Part(headers, content);
      parts + aPart
      nextPart = multipartStream.readBoundary();
    }
    parts.toList;
  }

  def part(headerName: String, headerValue: String): List[Part] = {
    parts.filter {
      p => p.headers.exists {
        h => h._1.equalsIgnoreCase(headerName) && h._2.equalsIgnoreCase(headerValue)
      }
    }
  }

  /**
   * Look for a part by name.  This assumes that the name of a part is
   * delivered as the substring ' name="partname"' of the Content-Disposition
   * header.
   * 
   * @param name - The name of the desired part.
   * @return a list of the parts whose name matches argument
   */
  def part(name: String): List[Part] = {
    parts.filter {
      p => p.headers.exists {
        h => h._1.equalsIgnoreCase("Content-Disposition") && h._2.indexOf(" name=\"" + name + "\"") != -1
      }
    }
  }

  class Part(val headers: List[(String, String)], val content: Content)
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
      } else if (lowerCaseMimeType.startsWith("text/xml") || lowerCaseMimeType.startsWith("text/xml")) {
        val str = 
          createStringContent(in) match {
            case None => ""
            case Some(sc) => sc.str
          }
        createXmlContent(str);
      } else if (lowerCaseMimeType.startsWith("multipart/form-data")) {
        createMultiPartContent(in, mimeType);
      } else {
        // Default to text/plain if some other Content-Type is specified;
        // todo Handle encodings
        if (in == null)
          None
        else
          createStringContent(in)
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

  // Make these classes?
  def createXmlContent(str: String): Option[XmlContent] = {
    try {
      // Now, we'll try to convert it to XML regardless of the Content-Type header.
      // We add an attribute to the xml so that our users don't have to.
      // Does it hurt if it's redundant???
      if (str.trim.length == 0)
        return None;

      val xml = scala.xml.XML.load(new StringReader(str));
      if (xml.namespace != null)
        Some(XmlContent(xml))
      else {
        val xmlns = new scala.xml.UnprefixedAttribute("xmlns", "http://www.pervasive.com/dispatcher", scala.xml.Null);
        Some(XmlContent(xml % xmlns))
      }
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
    // TODO: Default charset???
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
    val stream = {
      if (ContentCreator.log.isDebugEnabled) {
        val bytes = IOUtils.toByteArray(in);
        ContentCreator.log.debug("Multipart/form-data request's body contained " + bytes.length + " bytes.");
        val str = new String(bytes);

        ContentCreator.log.debug("Multipart/form-data request's body")
        ContentCreator.log.debug("----------------------------------")
        ContentCreator.log.debug(str)
        new ByteArrayInputStream(bytes)
      } else {
        in
      }
    }
    // TODO: Handle invalid headers
    val boundaryIndex = contentType.indexOf("boundary=");
    val boundaryString = contentType.substring(boundaryIndex + 9);
    val boundary = boundaryString.getBytes;
    Some(MultiPartContent(stream, boundary));
  }   
}