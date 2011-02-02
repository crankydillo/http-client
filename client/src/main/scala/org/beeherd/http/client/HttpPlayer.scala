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

import java.net.SocketTimeoutException

import org.joda.time.DateTime

import org.beeherd.dispatcher._
import org.beeherd.http.dispatcher._

import org.apache.http.conn.ConnectTimeoutException

trait HttpPlayer {
  def play(operations: Seq[Operation]): Seq[Tracked]
}

/**
* This class is provides a facility for issuing a bunch of Http Requests
* synchronously 
*
* @author scox
*/
class DelayingHttpPlayer(
    dispatcher: HttpDispatcher
    , delay: Int = 0
    , randomizeDelay: Boolean = false
  ) extends HttpPlayer {

  private def delayFn = 
    if (randomizeDelay) {
      val randomizer = new java.util.Random();
      () => randomizer.nextInt(delay) * 1000
    } else {
      () => delay * 1000
    }

  def play(operations: Seq[Operation]): Seq[Tracked] = {

    def now = System.currentTimeMillis

    def fn(op: Operation) = {
      val start = now

      def timeout = 
        Timeout(
          op.request.url
          , op.request.method
          , op.context
          , op.request.content match { case Some(c) => c.length; case _ => 0 }
          , now - start
        )

      try {
        val resp = dispatcher.dispatch(op.request);
        val respTime = now - start;

        val tracked = DResponse(
            op.request.url
            , op.request.method
            , op.context
            , op.request.content match { case Some(c) => c.length; case _ => 0 }
            , resp.code
            , respTime
            , resp.content match { case Some(c) => c.length; case _ => 0 }
          );

        op.handler match {
          case Some(h) => tracked +: h(resp).flatMap {op => play(Seq(op))}
          case _ => Seq(tracked)
        }

      } catch {
        case cte: ConnectTimeoutException => Seq(timeout)
        case ste: SocketTimeoutException => Seq(timeout)
      }
    }

    // We don't want to sleep on the first
    operations.take(1).flatMap { fn _ } ++
    operations.drop(1).flatMap { Thread.sleep(delayFn()); fn _ }
  }
}

class Operation(
    val request: HttpRequest
    , val handler: Option[(Response => Seq[Operation])] = None
    , val context: Option[String] = None
  )

// TODO: Seriously consider just carrying around Request/Response instances....
sealed abstract class Tracked(
    val url: String
    , val method: RequestMethod.Value
    , val context: Option[String] = None
    , val contentLength: Long = 0
  ) {
  // this is pretty bad, but I'm going to do it to get an idea of when the
  // request/response happened
  val requestDate = new DateTime();
}

case class Timeout(
    override val url: String
    , override val method: RequestMethod.Value
    , override val context: Option[String]
    , override val contentLength: Long = 0
    , time: Long
  ) extends Tracked(url, method, context, contentLength)

case class DResponse(
    override val url: String
    , override val method: RequestMethod.Value
    , override val context: Option[String]
    , override val contentLength: Long = 0
    , code: Int
    , responseTime: Long // in milliseconds
    , responseContentLength: Long = 0
  ) extends Tracked(url, method, context, contentLength)

/**
* A player that will randomized the top level operations.
*
* @author scox
*/
class RandomizingPlayer(
  sequentialPlayer: HttpPlayer
  , delay: Int = 0
) extends HttpPlayer {

  def play(operations: Seq[Operation]): Seq[Tracked] = 
    sequentialPlayer.play(scala.util.Random.shuffle(operations));
}

// Some formatting tools
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
      case DResponse(_, _, _, _, c, t, l) => (c, t, l)
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
        case DResponse(_, _, _, _, c, t, l) =>
          <response>
            <code>{c}</code>
            <responseTime>{t}</responseTime>
            <contentLength>{l}</contentLength>
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
