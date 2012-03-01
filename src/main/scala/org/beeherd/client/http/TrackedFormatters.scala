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
package org.beeherd.client.http

/**
* Implementations of this trait will format DResponses in various ways.
*/
trait TrackedFormatter {
  def format(tracked: Tracked): String
}

class SimpleTrackedFormatter(sep: String = ", ") extends TrackedFormatter {

  def format(tracked: Tracked): String = {
    val ctx = 
      tracked.context match {
        case Some(c) => c
        case _ => ""
      }

    val (code, time, contentLength) = tracked match {
      case DResponse(_, _, _, _, r, t) => (r.code, t, r.contentLength)
      case Timeout(_, _, _, _, t) => ("TIMEOUT", t, "")
    }

    tracked.requestDate + sep + tracked.method + sep + tracked.url + 
    sep + ctx + sep + tracked.contentLength  + sep + contentLength + 
    sep + time + sep + code
  }
}

class XmlTrackedFormatter extends TrackedFormatter {
  private val prettyPrinter = new scala.xml.PrettyPrinter(500, 2)
  def format(tracked: Tracked): String = {

    val request = 
      <request>
        <url>{tracked.url}</url>
        <method>{tracked.method}</method>
        <contentLength>{tracked.contentLength}</contentLength>
        {
          tracked.context match {
            case Some(ctx) => <context>{ctx}</context>
            case None => {}
          }
        }
      </request>

    val response = 
      tracked match {
        case DResponse(_, _, _, _, r, t) =>
          <response>
            <code>{r.code}</code>
            <responseTime>{t}</responseTime>
            <contentLength>{r.contentLength}</contentLength>
          </response>
        case Timeout(_, _, _, _, t) => <timeout>{t}</timeout>
      }

    prettyPrinter.format(
        <operation>
        <time>{tracked.requestDate}</time>
        {request}
        {response}
      </operation>
    )
  }
}
