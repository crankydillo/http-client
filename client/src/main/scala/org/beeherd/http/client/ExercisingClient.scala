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

import java.io.Writer
import java.util.{Timer, TimerTask}

import org.beeherd.dispatcher._
import org.beeherd.http.dispatcher._

/**
* This class exercises HTTP services.
*
* @author scox
*/
class ExercisingClient(
  player: HttpPlayer
  , secsToRun: Long
) {

  def exercise(operations: Seq[Operation], out: Writer): Unit = {
    // Do I need a blocking queue for this???
    var end: Boolean = false;

    val timer = new Timer();
    timer.schedule(
        new TimerTask { def run(): Unit = { end = true; timer.cancel() } }
        , secsToRun * 1000
      )

    val thread = new Thread(new Runnable {
          def run(): Unit = {
            while (!end) {
              val resp = player.play(operations);
              resp.foreach {r => out.write(r.toString + "\n")};
              out.flush();
            }
          }
        }, "Exerciser"
      );
    thread.start();
    thread.join();
  }
}
