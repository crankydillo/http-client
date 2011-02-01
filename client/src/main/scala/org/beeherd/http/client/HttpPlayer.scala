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
      def timeout = Timeout(op.request.url, op.context, now - start)
      try {
        val resp = dispatcher.dispatch(op.request);
        val respTime = now - start;

        val tracked = DResponse(op.request.url, op.context, resp.code, respTime);

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

sealed abstract class Tracked(val url: String, val context: Option[String] = None)

case class Timeout(
    override val url: String
    , override val context: Option[String]
    , time: Long
  ) extends Tracked(url, context)

case class DResponse(
    override val url: String
    , override val context: Option[String]
    , code: Int
    , responseTime: Long // in milliseconds
  ) extends Tracked(url, context)

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

class SimpleTrackedFormatter extends TrackedFormatter {
  def format(tracked: Tracked): String = {
    val ctx = 
      tracked.context match {
        case Some(c) => "; " + c
        case _ => ""
      }

    val (time, code) = tracked match {
      case DResponse(_, _, c, t) => ("; " + t, "; " + c)
      case Timeout(_, _, t) => ("; " + t, "")
    }

    tracked.url + ctx + time + code
  }
}

class XmlTrackedFormatter extends TrackedFormatter {
  private val prettyPrinter = new scala.xml.PrettyPrinter(500, 2)
  def format(tracked: Tracked): String = {

    val request = 
      <request>
        <url>{tracked.url}</url>
        {
          tracked.context match {
            case Some(ctx) => <context>{ctx}</context>
            case None => {}
          }
        }
      </request>

    val response = 
      tracked match {
        case DResponse(_, _, c, t) =>
          <response>
            <code>{c}</code>
            <responseTime>{t}</responseTime>
          </response>
        case Timeout(_, _, t) => <timeout>{t}</timeout>
      }

    prettyPrinter.format(<operation>{request}{response}</operation>)
  }
}
