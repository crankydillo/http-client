package org.beeherd.client.http

import java.io._
import java.nio.charset.Charset
import java.text.DecimalFormat
import java.util.concurrent.TimeUnit

import org.apache.commons.io.IOUtils

import org.apache.http.HttpEntity
import org.apache.http.conn.scheme.{Scheme, SchemeRegistry, PlainSocketFactory}
import org.apache.http.client.{HttpClient => ApacheHttpClient, HttpResponseException}
import org.apache.http.client.methods.{HttpGet, HttpPost, HttpPut, HttpDelete}
import org.apache.http.entity._
import org.apache.http.entity.mime._
import org.apache.http.entity.mime.content._
import org.apache.http.impl.client.{BasicResponseHandler, DefaultHttpClient}
import org.apache.http.impl.conn.tsccm.ThreadSafeClientConnManager
import org.apache.http.params.{BasicHttpParams, HttpConnectionParams}

import org.beeherd.client._

/**
* A convenience for issues HTTP requests.  It creates an HttpClient for
* each request.
*
* @author scox
*/
object HttpClient {
  val DefaultTimeout = 15000

  /**
  * Sets up all state for submitting the request and handles all the tear down.
  * <p>
  * This is not threadsafe!
  *
  * @param req
  * @return Response
  */
  def submit(req: HttpRequest, timeout: Int = DefaultTimeout): Response = 
    useClient(timeout) { _.submit(req) }

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
  ): Response = useClient(timeout) { _.get(url, params, headers) }

  private def useClient(timeout: Int = DefaultTimeout)
  (f: (HttpClient) => Response): Response = {
    val apacheClient = createClient(timeout);
    val client = new HttpClient(apacheClient);
    try {
      f(client);
    } finally {
      apacheClient.getConnectionManager.shutdown();
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

/**
* An Apache HttpClient wrapper.
* <p>
* <pre>
* {@code
* val client = new HttpClient(apacheClient)
* val xmlNodeData = 
*    client.get("http://your.rest.service/xmlResource") match {
*      case XmlResponse(xml) => (xml \ "data").text;
*      case _ => throw RuntimeException("Expected XML data");
*    }
* }
* <pre>
* @author scox
*/
class HttpClient(
    client: ApacheHttpClient
    , showProgress: Boolean = false
  ) {

  type Headers = Map[String, String]

  private val converter = new HttpConverter(showProgress); // compose!

  def submit(req: HttpRequest): Response = {
    try {
      req match {
          case HttpRequest(_, RequestMethod.Get) => get_h(req);
          case HttpRequest(_, RequestMethod.Post) => post_h(req);
          case HttpRequest(_, RequestMethod.Delete) => delete_h(req);
        }
    } catch {
      case e:HttpResponseException => {
        e.getStatusCode match {
          case 400 => BadResponse(e.getMessage)
          case 404 => NotFoundResponse
          case 403 => ForbiddenResponse(e.getMessage)
          case _ => InternalErrorResponse(e) // For now, just use this to wrap anything else
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
    submit(
      new HttpRequest(
        host
        , path
        , RequestMethod.Get
        , headers
        , params = params
        , protocol = protocol
        , port = port
      )
    )
  }

  private def get_h(req: HttpRequest): Response = {
    val meth = new HttpGet(req.url)

    try {
      val params = meth.getParams;
      req.params.foreach {e => params.setParameter(e._1, e._2)}

      req.headers.foreach {case (name, values) =>
        values.foreach {v => meth.addHeader(name, v)}
      }

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

      val (headers, entity) = createEntity(req);
      headers.foreach {case (n, v) => meth.addHeader(n, v)}
      meth.setEntity(entity);
      val handler = new BasicResponseHandler();
      val response = client.execute(meth);
      val body = handler.handleResponse(response);
      if (response.getEntity != null)
        response.getEntity.consumeContent;
      response.getHeaders("Location") match {
        case Array(fst, rest @_*) => CreatedResponse(fst.getValue)
        case _ => OkResponse
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
      val (headers, entity) = createEntity(req);
      headers.foreach {case (n, v) => meth.addHeader(n, v)}
      meth.setEntity(entity);
      val handler = new BasicResponseHandler();
      val response = client.execute(meth);
      val body = handler.handleResponse(response);
      if (response.getEntity != null)
        response.getEntity.consumeContent;
      response.getHeaders("Location") match {
        case Array(fst, rest @_*) => CreatedResponse(fst.getValue)
        case _ => OkResponse
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
      OkResponse
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

  private def createEntity(request: Request): (Headers, HttpEntity) = {

    def toEntity(content: Content): (Headers, HttpEntity) = {
      content match {
        case StringContent(str, ctype, charset) => 
          (Map(), new StringEntity(str, ctype, charset.name))
        case XmlContent(xml) =>
          (
              Map("Content-Type" -> "text/xml")
              , new StringEntity(xml.toString, "UTF-8")
            )
        case MultiPartContent(parts, _) => 
          val entity = new MultipartEntity(HttpMultipartMode.BROWSER_COMPATIBLE);
          parts.foreach {p => 
            val name = p.name;
            entity.addPart(
              name
              , p.content match {
                case StringContent(str, ctype, charset) => 
                  new StringBody(str, ctype, charset)
                case XmlContent(xml) => 
                  new StringBody(xml.toString, Charset.forName("UTF-8"))
                case ByteArrayContent(bytes) => new ByteArrayBody(bytes, name)
                case _ => new KnownLengthInputStreamBody(
                  new InputStreamBody(p.content.createStream, p.content.ctype)
                  , p.content.length
                )
              }
            )
          }
          (Map(), entity)
        case _ => (Map(), new StringEntity(""))
      }
    }

    request.content match {
      case Some(content) => toEntity(content);
      case _ => (Map(), new StringEntity(""))
    }
  }

  private[this] class KnownLengthInputStreamBody(delegate: InputStreamBody, length: Long) 
  extends ContentBody with ContentDescriptor {
    def getContentLength = length

    def getCharset = delegate.getCharset
    def getFilename = delegate.getFilename
    def getInputStream = delegate.getInputStream
    def getTransferEncoding = delegate.getTransferEncoding
    def writeTo(out: OutputStream) = delegate.writeTo(out)
    def writeTo(out: OutputStream, mode: Int) = delegate.writeTo(out, mode)
    def getMediaType = delegate.getMediaType
    def getMimeType = delegate.getMimeType
    def getSubType = delegate.getSubType
  }
}
