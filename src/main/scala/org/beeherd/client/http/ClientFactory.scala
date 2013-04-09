package org.beeherd.client.http

import javax.net.ssl.SSLContext

import org.apache.http.{
    HttpException, HttpHost, HttpRequest => ApacheHttpRequest, HttpRequestInterceptor
}
import org.apache.http.auth.{
    AuthScope, AuthState, UsernamePasswordCredentials
}

import org.apache.http.client.{
  CredentialsProvider, HttpClient => ApacheHttpClient
}
import org.apache.http.client.protocol.ClientContext
import org.apache.http.conn.ClientConnectionManager
import org.apache.http.conn.scheme.SchemeRegistry
import org.apache.http.conn.scheme.Scheme
import org.apache.http.conn.scheme.PlainSocketFactory
import org.apache.http.conn.ssl.SSLSocketFactory
import org.apache.http.impl.auth.BasicScheme
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.impl.conn.tsccm.ThreadSafeClientConnManager
import org.apache.http.params.HttpConnectionParams
import org.apache.http.params.BasicHttpParams
import org.apache.http.params.HttpParams
import org.apache.http.protocol.{
  ExecutionContext, HttpContext
}

object ClientFactory {
  private val instance = new ClientFactory(new DefaultHttpClientCreator)

  def createClient = instance.createClient

  def createClient(
    server: String
    , user: String
    , password: String
    , port: Int = 80
    , preemptive: Boolean = false
  ): ApacheHttpClient = instance.createClient(server, user, password, port, preemptive)

  // For unit testing the above
  class DefaultHttpClientCreator {
    def create(ccm: ClientConnectionManager, params: HttpParams) =
      new DefaultHttpClient(ccm, params)
  }

  class PreemptiveAuthInterceptor extends HttpRequestInterceptor {
    def process(request: ApacheHttpRequest , context: HttpContext): Unit = { 
      val authState = context.getAttribute(
        ClientContext.TARGET_AUTH_STATE).asInstanceOf[AuthState]

      // If no auth scheme avaialble yet, try to initialize it
      // preemptively
      if (authState.getAuthScheme() == null) {
        val credsProvider = context.getAttribute(
          ClientContext.CREDS_PROVIDER).asInstanceOf[CredentialsProvider]
        val targetHost = context.getAttribute(
          ExecutionContext.HTTP_TARGET_HOST).asInstanceOf[HttpHost]
        val creds = credsProvider.getCredentials(
          new AuthScope(targetHost.getHostName, targetHost.getPort))
        if (creds == null) 
          throw new HttpException("No credentials for preemptive authentication");
        authState.setAuthScheme(new BasicScheme);
        authState.setCredentials(creds);
      }   
    }   
  }
}

class ClientFactory(clientCreator: ClientFactory.DefaultHttpClientCreator) {

  def createClient = {
    val params = new BasicHttpParams()
    HttpConnectionParams.setSoTimeout(params, 15 * 1000)
    val schemeRegistry = new SchemeRegistry()
    schemeRegistry.register(
        new Scheme("http", PlainSocketFactory.getSocketFactory(), 80))

    val sslcontext = SSLContext.getInstance("TLS");
    sslcontext.init(null, null, null);
    val sf = new SSLSocketFactory(
      sslcontext, SSLSocketFactory.STRICT_HOSTNAME_VERIFIER)
    schemeRegistry.register(new Scheme("https", 443, sf))

    val cm = new ThreadSafeClientConnManager(params, schemeRegistry);
    new DefaultHttpClient(cm, params);
  }

  // should server and port be URL???
  def createClient(
    server: String
    , user: String
    , password: String
    , port: Int = 80
    , preemptive: Boolean = false
  ): DefaultHttpClient = {
    val client = createClient
    client.getCredentialsProvider().setCredentials(
      new AuthScope(server, port)
      , new UsernamePasswordCredentials(user, password)
    )

    if (preemptive) 
      client.addRequestInterceptor(new ClientFactory.PreemptiveAuthInterceptor, 0)

    client
  }
}
