package org.beeherd.client.http

import javax.net.ssl.SSLContext

import org.apache.http.params.HttpConnectionParams
import org.apache.http.params.BasicHttpParams
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.impl.conn.tsccm.ThreadSafeClientConnManager
import org.apache.http.conn.scheme.SchemeRegistry
import org.apache.http.conn.scheme.Scheme
import org.apache.http.conn.scheme.PlainSocketFactory
import org.apache.http.conn.ssl.SSLSocketFactory


object ClientFactory {
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
}
