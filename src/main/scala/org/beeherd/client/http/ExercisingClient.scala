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

import java.io.{BufferedWriter, OutputStreamWriter, Writer}
import java.util.{Timer, TimerTask}

import org.beeherd.client._

/**
* This class exercises HTTP services.
*
* @author scox
*/
class ExercisingClient(
  player: HttpPlayer
  , secsToRun: Long
  , operations: Seq[Operation]
  , out: Option[OutputDefinition] = None
) extends Runnable {

  def run(): Unit = {
    // Do I need a blocking queue for this???
    var end: Boolean = false;

    val timer = new Timer();
    timer.schedule(
        new TimerTask { def run(): Unit = { end = true; timer.cancel() } }
        , secsToRun * 1000
      )

    while (!end) {
      val responses = player.play(operations);

      out match {
        case Some(OutputDefinition(std, err, formatter)) => {
          responses.foreach {resp =>
            std.write(formatter.format(resp) + "\n");
            resp match {
              case DResponse(url, meth, _, _, rsp, time) =>
                if (rsp.code > 399) {
                  err.write("-" * 80 + "\n");
                  err.write(meth + " " + url + " - " + rsp.code + " (" +
                      (time / 1000.0) + " s)\n");
                  err.write(resp.requestDate + "\n");
                  err.write(rsp.content + "\n");
                }
              case _ =>
            }
          }
          std.flush();
          err.flush();
        }
        case _ =>
      }
    }
  }
}

/**
* Define some output stuff
*/
sealed case class OutputDefinition(
    val std: Writer
    , val err: Writer
    , val formatter: TrackedFormatter = new SimpleTrackedFormatter
  )
