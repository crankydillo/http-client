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
package org.beeherd.dispatcher

import java.nio.charset.Charset

import org.specs._
import org.specs.runner.JUnit4

class ContentSpecTest extends JUnit4(ContentSpec)
object ContentSpec extends Specification {

  "Content" should {

    "XmlContent" in {
      "be constructable from a string" in {

        "when the string can be parsed as XML" in {
          XmlContent("<a>bar</a>") match {
            case XmlContent(xml) => xml.text must beEqual("bar")
            case _ => fail("XmlContent was expected")
          }
        }

        "throw an IllegalArgumentException when it cannot be parsed as XML" in {
          XmlContent("invald") must throwA[IllegalArgumentException]
        }
      }
    }

    "StringContent" in {
      "default content type to text plain" in {
        StringContent("foo") match {
          case StringContent(_, ctype, _) => ctype must beEqual("text/plain")
          case _ => fail("Expected StringContent")
        }
      }

      "be constructable using a string for charset" in {
        StringContent("foo", "text/plain", "utf-8") match {
          case StringContent(_, _, cset) => 
            cset.name must beEqual(Charset.forName("utf-8").name)
          case _ => fail("Expected StringContent")
        }
      }

      "throw an IllegalArgumentException if the charset string is not valid" in {
        StringContent("foo", "text/plain", "blah") must 
        throwA[IllegalArgumentException]
      }
    }
  }
}
