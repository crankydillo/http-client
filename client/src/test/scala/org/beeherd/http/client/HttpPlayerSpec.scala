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

import org.specs.mock.Mockito
import org.mockito.Matchers._

import org.beeherd.dispatcher._

class HttpPlayerSpecTest extends JUnit4(HttpPlayerSpec)
object HttpPlayerSpec extends Specification with Mockito {

  "An DeletingHttpPlayer" should {
    "Issue a series of deletes for all CreatedResponses it received during a play" in {
      val underlying = mock[HttpPlayer]
      underlying.play(anyObject()) returns Seq()
    }
  }
}
