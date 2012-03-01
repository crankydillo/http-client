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

import java.net.SocketTimeoutException

import org.apache.http.conn.ConnectTimeoutException
import org.joda.time.DateTime

import org.beeherd.client._

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
    client: HttpClient
    , delay: Int = 0
    , randomizeDelay: Boolean = false
  ) extends HttpPlayer {

  private val randomizer = new java.util.Random();

  private def delayFn = {
    val time =
      if (randomizeDelay) randomizer.nextInt(delay) * 1000
      else delay * 1000
    Thread.sleep(time)
  }

  /**
  * <i>Play</i> the operations sequentially.
  *
  * @param operations
  */
  def play(operations: Seq[Operation]): Seq[Tracked] = {

    def now = System.currentTimeMillis

    def fn(op: Operation): Seq[Tracked] = {
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
        val resp = client.submit(op.request);
        val respTime = now - start;

        val tracked = DResponse(
            op.request.url
            , op.request.method
            , op.context
            , op.request.content match { case Some(c) => c.length; case _ => 0 }
            , resp
            , respTime
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
    fn(operations.head) ++ operations.drop(1).flatMap { delayFn; fn _ }
  }
}

/**
* An Operation represents a chain of HTTP requests where all but the first HTTP
* request is the result of applying some function to previous responses.
*/
class Operation(
    val request: HttpRequest
    , val handler: Option[(Response => Seq[Operation])] = None
    , val context: Option[String] = None
  ) {

  def this(request: HttpRequest, handler: ResponseHandler, context: String) =
    this(
      request
      , if (handler == null) None
        else Some(handler.handle _)
      , if (context == null) None
        else Some(context)
    )
}

trait ResponseHandler {
  def handle(resp: Response): Seq[Operation]
}

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
    , response: Response
    , responseTime: Long // in milliseconds
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

/**
* A player that will keep track of and delete created resources.
*/
class CreationTrackingPlayer(underlying: HttpPlayer) extends HttpPlayer {
  private val createdUrlss = new scala.collection.mutable.ArrayBuffer[String]();

  /**
  * <i>Play</i> a series of HTTP requests.  The URLs of created resources, which
  * are returned in the Location header of 201 responses are tracked.
  * 
  * @param operations
  */
  def play(operations: Seq[Operation]): Seq[Tracked] = {
    val results = underlying.play(operations);
    results.foreach {tracked =>
      tracked match {
        case DResponse(_, _, _, _, r, _) =>
          r match {
            case CreatedResponse(l, _) => createdUrlss += l
            case _ => 
          }
        case _ =>
      }
    }
    results
  }

  /**
  * Issue deletes for everything that has been created by this player.
  */
  def deleteAll(): Seq[Tracked] = 
    play(createdUrlss.map {u => 
      val (protocol, host, port, path) = HttpRequest.parseUrl(u);
      new Operation(new HttpRequest(host, path, 
          RequestMethod.Delete, protocol = protocol, port = port))
    }
  )

  /**
  * Return an immutable copy of the resource URLs that have been created by this
  * player.
  */
  def createdUrls = createdUrlss.toSeq
}



