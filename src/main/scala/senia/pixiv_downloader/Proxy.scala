package senia.pixiv_downloader

case class Proxy(name: String, port: Option[Int] = None, protocol: Option[String] = None, login_password: Option[(String, String)] = None) {
  def actualPort = port.getOrElse(8080)

  lazy val host: HttpHost = new HttpHost(name, actualPort, protocol.getOrElse("http"))
}
