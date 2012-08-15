package senia.pixiv_downloader.pixiv

import org.apache.http.impl.client.{BasicResponseHandler, DefaultHttpClient}
import org.apache.http.client.methods.{HttpPost, HttpGet}
import org.apache.http.{HttpStatus, Consts, NameValuePair}
import org.apache.http.message.BasicNameValuePair
import org.apache.http.client.entity.UrlEncodedFormEntity
import scalaz.Scalaz._
import org.apache.http.conn.params.ConnRoutePNames
import org.apache.http.auth.{UsernamePasswordCredentials, AuthScope}
import util.control.Exception._
import senia.pixiv_downloader.Config
import scala.collection.JavaConverters._

object Client {
  def withClient[T](config: Config)(f: DefaultHttpClient => T): T = {
    val client = new DefaultHttpClient()

    for {proxy <- config.proxy}
      client.getParams.setParameter(ConnRoutePNames.DEFAULT_PROXY, proxy.host)

    for {
      proxy <- config.proxy
      (login, password) <- proxy.login_password
    } client.getCredentialsProvider.setCredentials(
      new AuthScope(proxy.name, proxy.actualPort),
      new UsernamePasswordCredentials(login, password))

    allCatch.andFinally {
      client.getConnectionManager.shutdown()
    } {
      f(client)
    }
  }

  def getLoginPage(implicit client: DefaultHttpClient) = {
    val response = client execute new HttpGet(Url.login)
    new BasicResponseHandler handleResponse response // body
  }

  def getLoginRedirect(login: String, password: String)(implicit client: DefaultHttpClient): ValidationNEL[String, String] = {
    val http_post = new HttpPost(Url.login)
    val params = List[NameValuePair](
      new BasicNameValuePair("pixiv_id", login),
      new BasicNameValuePair("pass", password),
      new BasicNameValuePair("mode", "login"),
      new BasicNameValuePair("skip", "1")
    )
    http_post.setEntity(new UrlEncodedFormEntity(params.asJava, Consts.UTF_8))

    val response = client.execute(http_post /*, context*/)

    response.getStatusLine.getStatusCode match {
      case HttpStatus.SC_MOVED_TEMPORARILY =>
        response.getHeaders("location").toList match {
          case l :: Nil => l.getValue.successNel
          case head :: tail => (head :: tail).map {
            _.getValue
          }.mkString("Multiple locations: ", ",", ".").failNel
          case Nil => "No location found.".failNel
        }
      case x => ("Unsucceed status code: " + x).failNel
    }
  }
}
