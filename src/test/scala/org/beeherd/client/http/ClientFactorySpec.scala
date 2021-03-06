/**
* Copyright 2013 Samuel Cox
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

class ClientFactorySpec extends SpecificationWithJUnit with Mockito {
  import scala.collection.JavaConversions._

  "A ClientFactory" should {
    "create DefaultHttpClients" in {
    }

    "create DefaultHttpClients that will do preemptive authentication" in {
    }
  }
}


