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

import org.specs._
import org.specs.runner.JUnit4
import org.specs.mock.Mockito
import org.mockito.Matchers._

import org.apache.http.client.{HttpClient => ApacheHttpClient}

import org.beeherd.client._

class HttpClientSpecTest extends JUnit4(HttpClientSpec)
object HttpClientSpec extends Specification with Mockito {
  "The HttpClient" should {
    "allow GET HttpRequest submission" in {
      val apacheClient = mock[ApacheHttpClient]
      val client = new HttpClient(apacheClient);
      val req = new HttpRequest("hi");
      val resp = client.submit(req);
    }
  }
}
