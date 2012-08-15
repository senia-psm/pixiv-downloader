package senia.pixiv_downloader.pixiv

object Client {
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
