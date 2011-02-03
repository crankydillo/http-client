/*
 * Response.scala
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package org.beeherd.dispatcher

import javax.xml.parsers.{DocumentBuilderFactory,SAXParser, SAXParserFactory};
import javax.xml.transform.{Transformer, TransformerFactory};
import javax.xml.transform.sax.{SAXSource};
import javax.xml.transform.stream.{StreamSource, StreamResult};

import org.xml.sax.InputSource;

// The return type of RequestHandler.handle(Request)
// TODO: Much of this could be shared with Request....
class Response(val code: Int
  , val content: Option[Content]
  , hheaders: List[(String, String)]
) {
  override def toString = "Response (Code : " + code + "; Content: " + content + "; Headers: " + headers + ")";

  // I'm doing this to add the content length if it wasn't specified.  I'm not
  // sure how I feel about this.
  def headers: List[(String, String)] = {
    // Can you have more than one header with the same name???
    val contentHeaders = content match {
      case Some(c) => 
        List(
          "Content-Length" -> c.length.toString
          , "Content-Type"   -> c.ctype
        )
      case None => Nil
    }
    contentHeaders ++ hheaders
  }

  /**
   * Find the <b>first</b> header with a matching name.
   */
  def header(name: String, ignoreCase: Boolean): Option[String] = {
    val sameName = 
      if (ignoreCase)
        name.equalsIgnoreCase(_)
      else
        name.equals(_)

    headers.find { p => sameName(p._1) }.map(_._2)

  }

  /**
   * Find the <b>first</b> header with a matching name (case-sensitive).
   */
  def header(name: String): Option[String] = header(name, false)


  /**
   * @return the content length.  If there is no content, return 0.
   */
   lazy val contentLength = 
   content match { case Some(c) => c.length; case None => 0}
}

// Case classes appear to be controversial.  You can probably safely think of
// these as extensions that allow pattern matching.
case class OkResponse extends Response(200, None, Nil)

case class CreatedResponse(
    url: String
    , cntent: Option[Content]
  ) extends Response(201, cntent, List(("Location", url))) {
}
object CreatedResponse {
  def apply(url: String): CreatedResponse = CreatedResponse(url, None)
}

case class XmlResponse(xml: scala.xml.Node) 
extends Response(200, Some(XmlContent(xml)), Nil)

case class XHtmlResponse(xml: scala.xml.Node) 
extends Response(200, Some(XHtmlContent(xml)), Nil)

case class HtmlResponse(str: String) 
extends Response(200, Some(HtmlContent(str)), Nil)

case class StringResponse(str: String) extends Response(200, Some(StringContent(str, "text/plain")), Nil)

case class JsonResponse(xml: scala.xml.Node) extends Response(200,
    Some(StringContent(XMLJsonTransformer.transform(xml), "application/json")), Nil
  )

case class ZipResponse(bytes: Array[Byte]) extends Response(200,
    Some(ZipContent(bytes)), Nil
  )

case class ByteResponse(bytes: Array[Byte]) extends Response(200,
    Some(ByteArrayContent(bytes)), Nil
  )

case class StreamedResponse(val ctent: StreamedContent) extends Response(200,
    Some(ctent), Nil
  )
    
case class NotFoundResponse extends Response(404, None, Nil)

case class ForbiddenResponse(msg: String) extends Response(403, 
    Some(StringContent(msg, "text/plain")), Nil
  )

case class UnauthorizedResponse(msg: String) extends Response(401,
    Some(StringContent(msg, "text/plain")), Nil
  )

case class MethodNotAllowedResponse(supportedOps: List[String])
extends Response(405, None, {
      if (supportedOps == null || supportedOps.length == 0) {
        Nil
      } else {
        // todo: use Enum???
        var opStr = "";
        for (op <- supportedOps)
          opStr = opStr + ", " + op;
        opStr = opStr.substring(2)
        List(("Allow", opStr))
      }
    }
  )

object InternalErrorResponse {
  // Way to have another constructor
  def apply(throwable: Throwable): InternalErrorResponse = {
    val out = new java.io.StringWriter();
    throwable.printStackTrace(new java.io.PrintWriter(out));
    InternalErrorResponse(out.toString())
  }
}
case class InternalErrorResponse(message: String) 
extends Response(500, Some(StringContent(message, "text/plain")), Nil)


object XMLJsonTransformer {
  private val xslt = """<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:fo="http://www.w3.org/1999/XSL/Format" xmlns:ebl="urn:ebay:apis:eBLBaseComponents" exclude-result-prefixes="ebl">
	<!--====================================================================================
	Original version by : Holten Norris ( holtennorris at yahoo.com )
	Current version maintained  by: Alan Lewis (alanlewis at gmail.com)
	Thanks to Venu Reddy from eBay XSLT team for help with the array detection code
	Protected by CDDL open source license.  
	Transforms XML into JavaScript objects, using a JSON format.
	===================================================================================== -->
	<xsl:output method="text" encoding="UTF-8"/>
	<xsl:strip-space elements="*"/>
	<xsl:template match="*">
		<xsl:param name="recursionCnt">0</xsl:param>
		<xsl:param name="isLast">1</xsl:param>
		<xsl:param name="inArray">0</xsl:param>
		<xsl:if test="$recursionCnt=0">
			<xsl:text>{</xsl:text>
		</xsl:if>
		<!-- test what type of data to output  -->
		<xsl:variable name="elementDataType">
			<xsl:value-of select="number(text())"/>
		</xsl:variable>
		<xsl:variable name="elementData">
			<!-- TEXT ( use quotes ) -->
			<xsl:if test="string($elementDataType) ='NaN'">
				<xsl:if test="boolean(text())">
				<xsl:text/>"<xsl:value-of select="text()"/>"<xsl:text/>
				</xsl:if>
			</xsl:if>
			<!-- NUMBER (no quotes ) -->
			<xsl:if test="string($elementDataType) !='NaN'">
				<xsl:text/><xsl:value-of select="text()"/><xsl:text/>
			</xsl:if>
			<!-- NULL -->
			<xsl:if test="not(*)">
				<xsl:if test="not(text())">
					<xsl:text/>null<xsl:text/>
				</xsl:if>
			</xsl:if>
		</xsl:variable>
		<xsl:variable name="hasRepeatElements">
			<xsl:for-each select="*">
				<xsl:if test="name() = name(preceding-sibling::*) or name() = name(following-sibling::*)">
					<xsl:text/>true<xsl:text/>
				</xsl:if>
			</xsl:for-each>
		</xsl:variable>
		<xsl:if test="not(count(@*) &gt; 0)">
		 <xsl:text/>"<xsl:value-of select="local-name()"/>":<xsl:value-of select="$elementData"/><xsl:text/>
		</xsl:if>
		<xsl:if test="count(@*) &gt; 0">
		<xsl:text/>"<xsl:value-of select="local-name()"/>":{"content":<xsl:value-of select="$elementData"/><xsl:text/>
			<xsl:for-each select="@*">
				<xsl:if test="position()=1">,</xsl:if>
				<!-- test what type of data to output  -->
				<xsl:variable name="dataType">
					<xsl:text/><xsl:value-of select="number(.)"/><xsl:text/>
				</xsl:variable>
				<xsl:variable name="data">
					<!-- TEXT ( use quotes ) -->
					<xsl:if test="string($dataType) ='NaN'">
				<xsl:text/>"<xsl:value-of select="current()"/>"<xsl:text/> </xsl:if>
					<!-- NUMBER (no quotes ) -->
					<xsl:if test="string($dataType) !='NaN'">
						<xsl:text/><xsl:value-of select="current()"/><xsl:text/>
					</xsl:if>
				</xsl:variable>
				<xsl:text/><xsl:value-of select="local-name()"/>:<xsl:value-of select="$data"/><xsl:text/>
				<xsl:if test="position() !=last()">,</xsl:if>
			</xsl:for-each>
		<xsl:text/>}<xsl:text/>
		</xsl:if>
		<xsl:if test="not($hasRepeatElements = '')">
					<xsl:text/>[{<xsl:text/>
				</xsl:if>
		<xsl:for-each select="*">
			<xsl:if test="position()=1">
				<xsl:if test="$hasRepeatElements = ''">
					<xsl:text>{</xsl:text>
				</xsl:if>
			</xsl:if>
			<xsl:apply-templates select="current()">
				<xsl:with-param name="recursionCnt" select="$recursionCnt+1"/>
				<xsl:with-param name="isLast" select="position()=last()"/>
				<xsl:with-param name="inArray" select="not($hasRepeatElements = '')"/>
			</xsl:apply-templates>
			<xsl:if test="position()=last()">
				<xsl:if test="$hasRepeatElements = ''">
					<xsl:text>}</xsl:text>
				</xsl:if>
			</xsl:if>
		</xsl:for-each>
		<xsl:if test="not($hasRepeatElements = '')">
					<xsl:text/>}]<xsl:text/>
				</xsl:if>
		<xsl:if test="not( $isLast )">
			<xsl:if test="$inArray = 'true'">
				<xsl:text>}</xsl:text>
			</xsl:if>
			<xsl:text/>,<xsl:text/>
			<xsl:if test="$inArray = 'true'">
				<xsl:text>{</xsl:text>
			</xsl:if>
		</xsl:if>
		<xsl:if test="$recursionCnt=0">}</xsl:if>
	</xsl:template>
</xsl:stylesheet>
"""


  private val transformer = {
    val fac = DocumentBuilderFactory.newInstance;
    val tfac = TransformerFactory.newInstance;

    val builder = fac.newDocumentBuilder;

    // todo: extract this to a file so you can modify it.
    val stylesheet = new StreamSource(new java.io.StringReader(xslt));
    tfac.newTransformer(stylesheet);
  }

  def transform(xml: scala.xml.Node): String = {
      val source = new SAXSource(new InputSource(
          new java.io.StringReader(xml.toString)));

      val str = new java.io.StringWriter;
      val out = new StreamResult(str);
      transformer.transform(source, out);
      str.toString;
  }
}

