package org.beeherd.http.client

import org.apache.http.params.HttpConnectionParams;
import org.apache.http.params.BasicHttpParams
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.impl.conn.tsccm.ThreadSafeClientConnManager
import org.apache.http.conn.scheme.SchemeRegistry
import org.apache.http.conn.scheme.Scheme
import org.apache.http.conn.scheme.PlainSocketFactory

object ClientFactory {
  def createClient = {
    val params = new BasicHttpParams();
    HttpConnectionParams.setSoTimeout(params, 15 * 1000);
    val schemeRegistry = new SchemeRegistry();
    schemeRegistry.register(
        new Scheme("http", PlainSocketFactory.getSocketFactory(), 80));

    val cm = new ThreadSafeClientConnManager(params, schemeRegistry);
    new DefaultHttpClient(cm, params);
  }
}
