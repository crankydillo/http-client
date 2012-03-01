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

import org.specs._
import org.specs.runner.JUnit4

class SimpleTrackedParserSpecTest extends JUnit4(SimpleTrackedParserSpec)
object SimpleTrackedParserSpec extends Specification {

  "The SimpledTackedParser" should {
    "produce some statistics based on the lines produced by SimpleTrackedFormatter" in {
      val stats = SimpleTrackedParser.parseStats(lines.split("\n"));
      stats.contextStats must haveSize(3)

      stats.numRequests must beEqual(6)
      stats.numErrors must beEqual(1)

      val batch = stats.contextStats("batch/schedules");
      batch.numRequests must beEqual(3);
      batch.numErrors must beEqual(1);
      batch.averageResponseTime must beEqual(200.0)
      batch.worstResponseTime must beEqual(300)
      batch.bytesSent must beEqual(0)
      batch.bytesReceived must beEqual(600)
      batch.numPosts must beEqual(0);
      batch.numGets must beEqual(3);

      val logger = stats.contextStats("logger");
      logger.numPosts must beEqual(2)

      val unclassified = stats.contextStats("");
      unclassified.numRequests must beEqual(1);
    }
  }

  private val lines = """
2011-02-02T06:01:13.029-06:00, Get, http://foo:80/di/services/batch/schedules, batch/schedules, 0, 100, 100, 200
2011-02-02T06:01:14.110-06:00, Post, http://foo:80/di/services/logger, logger, 100, 53, 80, 200 
2011-02-02T06:01:14.110-06:00, Post, http://foo:80/di/services/logger, , 100, 53, 80, 200 
2011-02-02T06:01:14.110-06:00, Post, http://foo:80/di/services/logger, logger, 200, 23, 33, 200 
2011-02-02T06:01:13.029-06:00, Get, http://foo:80/di/services/batch/schedules, batch/schedules, 0, 200, 200, 401
2011-02-02T06:01:13.029-06:00, Get, http://foo:80/di/services/batch/schedules, batch/schedules, 0, 300, 300, 201
""".trim

}
