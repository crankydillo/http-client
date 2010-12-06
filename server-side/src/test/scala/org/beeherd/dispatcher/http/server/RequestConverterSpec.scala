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
package org.beeherd.dispatcher.http.server

import javax.servlet.http.HttpServletRequest

import org.specs._
import org.specs.runner.JUnit4
import org.specs.mock.Mockito
import org.mockito.Matchers._ 

import org.beeherd.dispatcher.http._

class RequestConverterSpecTest extends JUnit4(RequestConverterSpec)
object RequestConverterSpec extends Specification with Mockito {

  import scala.collection.JavaConversions._


  val converter = new RequestConverter()

  "The RequestConverter" should {
    "create an HttpRequest with values within an HttpServletRequest" in {
      "convert a GET request" in {
        val get = MockFactory.mockGetRequest;
        val req = converter.convert(get);
        req must notBeNull

        req.method must beEqual(RequestMethod.Get)

        val headers = req.headers;
        headers must notBeNull;
        headers must havePair(("Content-Type", List("text/xml")));
        headers must havePair(("Accept", List("text/xml", "text/plain")));
      }

      "convert a POST request with XML content" in {
        val req = converter.convert(MockFactory.mockXmlPostRequest);
        req must notBeNull

        req.method must beEqual(RequestMethod.Post)

        val headers = req.headers;
        headers must notBeNull;
        headers must havePair(("Content-Type", List("text/xml")));
      }

      "not convert the input stream when it is above a certain size" in {
      }
    }
  }
}
