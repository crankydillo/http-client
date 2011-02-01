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

import scala.actors.Actor
import scala.actors.Actor._

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

  def exercise(operations: Seq[Operation]): Unit = {
  }
  
  case class Stop
  case class Start

  class PlayingActor(operations: Seq[Operation]) extends Actor {
    def act() {
      loop {
        react {
          case Stop => exit()
          case Start => {
            val resp = player.play(operations);
            println(resp)
          }
        }
      }
    }
  }
}
