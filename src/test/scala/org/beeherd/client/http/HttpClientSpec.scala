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
import org.specs.mock.Mockito
import org.mockito.ArgumentCaptor
import org.mockito.Mockito.{verify => mVerify}

import org.apache.http.Header
import org.apache.http.client.{HttpClient => ApacheHttpClient}
import org.apache.http.client.methods.{HttpGet, HttpPost, HttpPut, HttpDelete}

class HttpClientSpec extends SpecificationWithJUnit with Mockito {
  import scala.collection.JavaConversions._

  "The HttpClient" should {
    "support GET requests" in {

      "allow GET HttpRequest submission" in {
        val apacheClient = mock[ApacheHttpClient]
        val client = new HttpClient(apacheClient);
        val req = new HttpRequest("hi", headers = Map("h1" -> List("hv1")),
          params = Map("p1" -> "pv1"));

        client.submit(req);

        val methodCaptor = ArgumentCaptor.forClass(classOf[HttpGet]);
        mVerify(apacheClient).execute(methodCaptor.capture);
        val method = methodCaptor.getValue;
        method must notBeNull;

        "which should set the URI" in {
          method.getURI.toString must beEqual("http://hi:80/");
        }

        "and the headers" in {
          val headers = method.headerIterator().toList.asInstanceOf[List[Header]]
          headers must haveSize(1);
          headers(0).getName() must beEqual("h1")
          headers(0).getValue() must beEqual("hv1")
        }

        "and the parameters" in {
          val params = method.getParams;
          params must notBeNull;
          val p1Param = params.getParameter("p1");
          p1Param must notBeNull;
          val p1Val = p1Param.getValue;
          p1Val match {
            case s:String => s must beEqual("pv1")
            case _ => fail("Expected a string")
          }
        }
      }

      "through a get function" in {
        val apacheClient = mock[ApacheHttpClient]
        val client = new HttpClient(apacheClient);
        client.get("marcels")
                val methodCaptor = ArgumentCaptor.forClass(classOf[HttpGet]);
        mVerify(apacheClient).execute(methodCaptor.capture);
        val method = methodCaptor.getValue;
        method must notBeNull;
        method.getURI.toString must beEqual("http://marcels:80");
      }
    }

    "support POST requests" in {

    }

    "support PUT requests" in {

    }

    "support DELETE requests" in {

    }

    "support HEAD requests" in {

    }

    "support OPTIONS requests" in {

    }

    "support TRACE requests" in {

    }
  }
}
