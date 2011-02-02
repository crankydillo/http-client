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

import scala.io.Source

object SimpleTrackedParser {

  def parseStats(file: java.io.File): Stats = {
    val source = Source.fromFile(file);
    try {
      // fold or iterator??
      parseStats(source.getLines.toSeq)
    } finally {
      try {source.close()} catch { case e:Exception => {} }
    }
  }

  def parseStats(lines: Seq[String]): Stats = {
    // assumes the following comma delimited format:
    // datetime (XML standard), request method, url, context, request length,
    // response length, response time, code

    val trackedOutput = lines.map {l =>
      val arr = l.split(", ");
      TrackedOutput(
          arr(0).trim
          , arr(1).trim
          , arr(2).trim
          , arr(3).trim
          , arr(4).trim.toLong
          , arr(5).trim.toLong
          , arr(6).trim.toInt
          , arr(7).trim.toInt
        )
    }

    val contextGroups = trackedOutput.groupBy { _.context }

    val groupStats = contextGroups.mapValues {trackedOuts =>
      val (reqs, errors, respTime, worstRespTime, sent, received, gets, posts) =
        trackedOuts
        .foldLeft ((0, 0, 0L, 0, 0L, 0L, 0, 0))
        { case((reqs, errs, respTime, worstTime, sent, received, gets, posts), to) => 
          (
              reqs + 1
              , if (to.isError) errs + 1 else errs
              , respTime + to.responseTime
              , if (to.responseTime > worstTime) to.responseTime else worstTime
              , sent + to.requestLength
              , received + to.responseLength
              , if (to.method.toLowerCase == "get") gets + 1 else gets
              , if (to.method.toLowerCase == "post") posts + 1 else posts
            )
        };

      ContextStats(reqs, errors, respTime / (trackedOuts.size * 1.0), 
          worstRespTime, sent, received, gets, posts)
    }

    Stats(groupStats);
  }


  private[this] case class TrackedOutput (
      datetime: String // no need to parse...yet
      , method: String 
      , url: String
      , context: String
      , requestLength: Long
      , responseLength: Long
      , responseTime: Int
      , code: Int
    ) {
    lazy val isError = code >= 400
  }
}


case class Stats(
    contextStats: Map[String, ContextStats]
  ) {
  lazy val numRequests = contextStats.map {case (k, v) => v.numRequests}.sum
  lazy val numErrors = contextStats.map {case (k, v) => v.numErrors}.sum
  lazy val bytesSent = contextStats.map {case (k, v) => v.bytesSent}.sum
  lazy val bytesReceived = contextStats.map {case (k, v) => v.bytesReceived}.sum
  lazy val numGets = contextStats.map {case (k, v) => v.numGets}.sum
  lazy val numPosts = contextStats.map {case (k, v) => v.numPosts}.sum
}
case class ContextStats(
    numRequests: Int
    , numErrors: Int
    , averageResponseTime: Double
    , worstResponseTime: Long
    , bytesSent: Long
    , bytesReceived: Long
    , numGets: Int
    , numPosts: Int
  )
