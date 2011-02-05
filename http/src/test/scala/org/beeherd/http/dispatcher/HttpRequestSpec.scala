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
package org.beeherd.http.dispatcher

import org.specs._
import org.specs.runner.JUnit4

class HttpRequestSpecTest extends JUnit4(HttpRequestSpec)
object HttpRequestSpec extends Specification {
  "An HttpRequest" should {
    "be pattern matchable based on the path and method" in {
      val req = new HttpRequest("boo.com", "/some/path", RequestMethod.Get);

      req match {
        case HttpRequest("some" :: "path" :: Nil, RequestMethod.Get) => {
          true must beTrue
        }
        case _ => false must beTrue
      }
    }
  }

  "The HttpRequest object" should {
    import HttpRequest._
    "parse URLs into (protocol, host, port, path)" in {
      parseUrl("http://beeherd.org/foo/bar") must beEqual(
        ("http", "beeherd.org", 80, "/foo/bar"))
    }
  }
}
