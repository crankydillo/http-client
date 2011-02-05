package org.beeherd.http.client

import java.io._
import java.text.DecimalFormat
import java.util.concurrent.TimeUnit

import org.apache.commons.io.IOUtils
import org.apache.http.conn.scheme.{Scheme, SchemeRegistry, PlainSocketFactory}
import org.apache.http.client.{ResponseHandler, HttpClient => ApacheHttpClient, HttpResponseException}
import org.apache.http.client.methods.{HttpGet, HttpPost, HttpPut, HttpDelete}
import org.apache.http.entity.StringEntity
import org.apache.http.impl.client.{BasicResponseHandler, DefaultHttpClient}
import org.apache.http.impl.conn.tsccm.ThreadSafeClientConnManager
import org.apache.http.params.{BasicHttpParams, HttpConnectionParams}

import org.beeherd.dispatcher._
import org.beeherd.http.dispatcher._

object HttpDispatcher {
  val DefaultTimeout = 15000

  /**
  * Sets up all state for dispatching the request and handles all the tear down.
  * <p>
  * This is not threadsafe!
  *
  * @param req
  * @return Response
  */
  def dispatch(req: HttpRequest, timeout: Int = DefaultTimeout): Response = {
    val client = createClient(timeout)
    val dispatcher = new HttpDispatcher(client);
    try {
      dispatcher.dispatch(req);
    } finally {
      client.getConnectionManager.shutdown();
    }
  }

  /**
  * Sets up all state for issuing a get and handles all the tear down.
  * <p>
  * This is not threadsafe!
  *
  * @param url
  * @param params
  * @param headers
  * @return Response
  */
  def get(
    url: String
    , params: Map[String, String] = Map()
    , headers: Map[String, List[String]] = Map()
    , timeout: Int = DefaultTimeout
  ): Response = {
    val client = createClient(timeout)
    val dispatcher = new HttpDispatcher(client);
    try {
      dispatcher.get(url, params, headers)
    } finally {
      client.getConnectionManager.shutdown();
    }
  }

  private def createClient(timeout: Int) = {
    val params = new BasicHttpParams();
    HttpConnectionParams.setSoTimeout(params, timeout);
    val schemeRegistry = new SchemeRegistry();
    schemeRegistry.register(
        new Scheme("http", PlainSocketFactory.getSocketFactory(), 80));

    val cm = new ThreadSafeClientConnManager(params, schemeRegistry);
    new DefaultHttpClient(cm, params);
  }

}

class HttpDispatcher(
    client: ApacheHttpClient
    , showProgress: Boolean = false
  ) {

  private val converter = new HttpConverter(showProgress); // compose!

  def dispatch(req: HttpRequest): Response = {
    try {
      req match {
          case HttpRequest(_, RequestMethod.Get) => get_h(req);
          case HttpRequest(_, RequestMethod.Post) => post_h(req);
          case HttpRequest(_, RequestMethod.Delete) => delete_h(req);
        }
    } catch {
      case e:HttpResponseException => {
        e.getStatusCode match {
          case 404 => NotFoundResponse();
          case 403 => ForbiddenResponse(e.getMessage);
          case _ => InternalErrorResponse(e); // For now, just use this to wrap anything else
        }
      }
      case e:Exception => {
        InternalErrorResponse(e);
      }

    }
  }

  def get(
    url: String
    , params: Map[String, String] = Map()
    , headers: Map[String, List[String]] = Map()
  ): Response = {
    val (protocol, host, port, path) = HttpRequest.parseUrl(url);
    val req = 
      new HttpRequest(
        host
        , path
        , RequestMethod.Get
        , headers
        , params = params
        , protocol = protocol
        , port = port
      )
    dispatch(req)
  }

  private def get_h(req: HttpRequest): Response = {
    val meth = new HttpGet(req.url)

    try {
      val params = meth.getParams;
      req.params.foreach {e => params.setParameter(e._1, e._2)}
      val response = client.execute(meth);
      converter.convert(response);

    } catch {
      case e:Exception => {
        meth.abort;
        throw e
      }
    }
  }

  private def post_h(req: HttpRequest): Response = {
    val meth = new HttpPost(req.url)
    try {
      val params = meth.getParams;
      req.params.foreach {case (n, v) => params.setParameter(n, v)}

      req.headers.foreach {case (name, values) =>
        values.foreach {v => meth.addHeader(name, v)}
      }

      val entity = 
        if (req.content != None) {
        req.content.get match {
          case StringContent(str, ctype) => new StringEntity(str, ctype);
          case XmlContent(xml) => {
            meth.addHeader("Content-Type", "text/xml");
            new StringEntity(xml.toString, "UTF-8");
          }
          case _ => new StringEntity("");
        }
      } else {
        new StringEntity("");
      }
      meth.setEntity(entity);
      val handler = new BasicResponseHandler();
      val response = client.execute(meth);
      val body = handler.handleResponse(response);
      if (response.getEntity != null)
        response.getEntity.consumeContent;
      response.getHeaders("Location") match {
        case Array(fst, rest @_*) => CreatedResponse(fst.getValue)
        case _ => OkResponse()
      }
    } catch {
      case e:Exception => {
        meth.abort;
        throw e;
      }
    }
  }

  private def put_h(req: HttpRequest): Response = {
    val meth = new HttpPut(req.url)
    try {
      val params = meth.getParams;
      req.params.foreach {e => params.setParameter(e._1, e._2)}
      val entity = 
        if (req.content != None) {
        req.content.get match {
          case StringContent(str, ctype) => new StringEntity(str, ctype);
          case XmlContent(xml) => {
            meth.addHeader("Content-Type", "text/xml");
            new StringEntity(xml.toString, "UTF-8");
          }
          case _ => new StringEntity("");
        }
      } else {
        new StringEntity("");
      }
      meth.setEntity(entity);
      val handler = new BasicResponseHandler();
      val response = client.execute(meth);
      val body = handler.handleResponse(response);
      if (response.getEntity != null)
        response.getEntity.consumeContent;
      response.getHeaders("Location") match {
        case Array(fst, rest @_*) => CreatedResponse(fst.getValue)
        case _ => OkResponse()
      }
    } catch {
      case e:Exception => {
        meth.abort;
        throw e;
      }
    }
  }

  private def delete_h(req: HttpRequest): Response = {
    val meth = new HttpDelete(req.url);
    try {
      val handler = new BasicResponseHandler();
      val response = client.execute(meth);
      if (response.getEntity != null)
        response.getEntity.consumeContent;
      val body = handler.handleResponse(response);
      OkResponse()
    } catch {
      case e:Exception => {
        meth.abort;
        throw e;
      }
    }
  }

  def download(req: HttpRequest, timeout: Int, target: File): Unit = {

    def toKiloDecimal(d: Double, precision: Int = 2): String =
      new DecimalFormat("0." + "0" * precision).format(d / 1024.0)
    def toDecimal(d: Double, precision: Int = 2): String =
      new DecimalFormat("0." + "0" * precision).format(d)

    def toTime(millis: Long) = 
      String.format("%02d:%02d", 
        TimeUnit.MILLISECONDS.toMinutes(millis).asInstanceOf[AnyRef],
        (TimeUnit.MILLISECONDS.toSeconds(millis) - 
        TimeUnit.MINUTES.toSeconds(TimeUnit.MILLISECONDS.toMinutes(millis))).asInstanceOf[AnyRef]
      );

    require(target != null);
    require(!(target.exists && target.isDirectory));

    val meth = new HttpGet(req.url);

    var in:InputStream = null;
    var out: OutputStream = new BufferedOutputStream(new FileOutputStream(target));
    try {
      val params = meth.getParams;
      req.params.foreach {e => params.setParameter(e._1, e._2)}
      val response = client.execute(meth);
      in = new BufferedInputStream(response.getEntity.getContent);
      if (showProgress) {
        val contentLengthOpt = {
          val header = response.getFirstHeader("Content-Length");
          if (header == null) {
            None
          } else {
            try {
              val len = header.getValue.trim.toLong
              Some(
                (len, toKiloDecimal(len))
              )
            } catch {
              case _ => None
            }
          }
        }

        val begin = "Downloading: ";
        val middle = "/" + (contentLengthOpt match { 
            case Some((_, s)) => s
            case _ => "???"
          }
        )
        var ctr = 0;
        print("0" + middle);

        var abyte = in.read();
        val start = System.currentTimeMillis;
        while (abyte != -1) {
          if (ctr % 1024 == 0) {
            print("\r"); // guaranteed to always get bigger...
            val kilos = (ctr / 1024) + ""
            val end = contentLengthOpt match {
              case Some((l, _)) => " (" + toDecimal((ctr / (l * 1.0)) * 100, 2) + "%)"
              case _ => ""
            }
            val time = toTime(System.currentTimeMillis - start);
            print(begin + kilos + middle + end + " " + time);
          }
          out.write(abyte);
          abyte = in.read
          ctr = ctr + 1;
        }
        println();
      } else
        IOUtils.copy(in, out);

      out.flush
    } finally {
      try {in.close()} catch {case e:Exception => {}}
      try {out.close()} catch {case e:Exception => {}}
    }
  }

}
