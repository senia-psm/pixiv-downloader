package senia.pixiv_downloader

import org.apache.http.client.ResponseHandler
import org.apache.http.client.HttpClient
import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.BasicResponseHandler
import org.apache.http.impl.client.DefaultHttpClient
import scala.util.control.Exception.allCatch

object Test extends App {

  def withClient[T](f: HttpClient => T): T = {
    val httpclient = new DefaultHttpClient()
    allCatch.andFinally{ httpclient.getConnectionManager().shutdown() }{ f(httpclient) }
  }

  withClient{ client =>
    val httpget = new HttpGet("http://www.google.com/")
    val responseHandler = new BasicResponseHandler()
    val handler: ResponseHandler[String] = new BasicResponseHandler
    val response = client execute httpget
    val body = handler handleResponse response
    println(body)
  }
}
