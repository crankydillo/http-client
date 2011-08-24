/**
* Copyright 2010 Samuel Cox
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing,
* software distributed under the License is distributed on an
* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
* KIND, either express or implied. See the License for the
* specific language governing permissions and limitations
* under the License.
*/
package org.beeherd.http.client

import java.io._
import java.text.DecimalFormat
import java.util.concurrent.TimeUnit

import org.apache.commons.io.IOUtils
import org.apache.http.HttpResponse
import org.apache.http.conn.scheme.{Scheme, SchemeRegistry, PlainSocketFactory}
import org.apache.http.client.{HttpClient => ApacheHttpClient, HttpResponseException}
import org.apache.http.client.methods.{HttpGet, HttpPost, HttpPut, HttpDelete}
import org.apache.http.entity.StringEntity
import org.apache.http.impl.client.{BasicResponseHandler, DefaultHttpClient}
import org.apache.http.impl.conn.tsccm.ThreadSafeClientConnManager
import org.apache.http.params.{BasicHttpParams, HttpConnectionParams}

import org.beeherd.dispatcher._
import org.beeherd.http.dispatcher._

class HttpConverter(showProgress: Boolean = false) {
  def convert(apacheResponse: HttpResponse): Response = {
    val contentType = apacheResponse.getFirstHeader("Content-Type").getValue.trim;
    if (contentType == null) {
      StringResponse("")
    } else if (contentType.startsWith("text/xml") || contentType.startsWith("application/xml")) {
      val xml = scala.xml.XML.load(new java.io.StringReader(getString(apacheResponse)));
      XmlResponse(xml)
    } else if ("text/html" == contentType) {
      HtmlResponse(getString(apacheResponse))
    } else if ("application/zip" == contentType) {
      ZipResponse(getBytes(apacheResponse))
    } else {
      StringResponse(getString(apacheResponse));
    }
  }

  private def getString(response: HttpResponse): String = {
    val handler = new BasicResponseHandler();
    val str = handler.handleResponse(response);
    if (response.getEntity != null)
      response.getEntity.consumeContent;
    str
  }

  private def getBytes(response: HttpResponse): Array[Byte] = {
    if (response.getEntity != null) {
      var in: InputStream = null;
      var out: OutputStream = null;
      try {
        val in = response.getEntity.getContent;
        val out = new ByteArrayOutputStream();
        if (showProgress) {
          val contentLengthOpt = {
            val header = response.getFirstHeader("Content-Length");
            if (header == null)
              None
            else 
              try {
              Some(new DecimalFormat("#.##").format(
                  header.getValue.trim.toLong / 1024.0))
            } catch {
              case _ => None
            }
          }
          println();
          val ending = "/" + contentLengthOpt.getOrElse("???")
            var ctr = 0;
          var lastPrint = "0";
          print(lastPrint + ending);
          var abyte = in.read();
          while (abyte != -1) {
            if (ctr % 1024 == 0) {
              print("\r"); // guaranteed to always get bigger...
              lastPrint = (ctr / 1024) + ""
              print(lastPrint + ending);
            }
            abyte = in.read
            out.write(abyte);
            ctr = ctr + 1;
          }
        } else
        IOUtils.copy(in, out);

        out.flush
        out.close
        out.toByteArray
      } finally {
        if (in != null) 
          try {in.close} catch { case e:Exception => {}}
      }
    } else {
      Array()
    }
  }


}
