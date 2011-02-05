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

import java.io.{BufferedWriter, File, FileWriter, OutputStreamWriter}

import org.apache.http.conn.scheme.{Scheme, SchemeRegistry, PlainSocketFactory}
import org.apache.http.client.{ResponseHandler, HttpClient => ApacheHttpClient, HttpResponseException}
import org.apache.http.client.methods.{HttpGet, HttpPost, HttpPut, HttpDelete}
import org.apache.http.entity.StringEntity
import org.apache.http.impl.client.{BasicResponseHandler, DefaultHttpClient}
import org.apache.http.impl.conn.tsccm.ThreadSafeClientConnManager
import org.apache.http.params.{BasicHttpParams, HttpConnectionParams}

import org.beeherd.http.dispatcher._

object App {
  def main(args: Array[String]): Unit = {
    try {

      val timeout = args match {
        case Array(fst, snd @_*) => fst.toInt
        case _ => 60000
      }

      val params = new BasicHttpParams();
      HttpConnectionParams.setSoTimeout(params, timeout)
      val schemeRegistry = new SchemeRegistry();
      schemeRegistry.register(
          new Scheme("http", PlainSocketFactory.getSocketFactory(), 80));

      val cm = new ThreadSafeClientConnManager(params, schemeRegistry);
      val client = new DefaultHttpClient(cm, params);
      val dispatcher = new HttpDispatcher(client);

      val player = new RandomizingPlayer(new DelayingHttpPlayer(dispatcher, 3, true));
      try {
        val out = new BufferedWriter(new OutputStreamWriter(Console.out));
        val err = new BufferedWriter(new FileWriter(new File("errs")))
        val exerciser = new ExercisingClient(player, 15, Ops,
          Some(OutputDefinition(out, err)))
        val thread = new Thread(exerciser, "Exerciser");
        thread.start();
        thread.join();
        /*
        val lst = player.play(Ops);
        lst.foreach {resp =>
          resp match {
            case Timeout() => println("The request timed out.");
            case DResponse(url, code, time) => 
              println(url + "; " + code + "; " + time)
          }
        }
        */
      } finally {
        client.getConnectionManager.shutdown();
      }
    } catch {
      case t:Throwable => t.printStackTrace
    }
  }

  def Ops = Seq(
      new Operation(new HttpRequest("www.cnn.com"))
      , new Operation(new HttpRequest("www.slashdot.com"))
      , new Operation(new HttpRequest("www.booyea~oejif.com"))
    )
}
