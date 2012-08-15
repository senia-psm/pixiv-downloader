package senia.pixiv_downloader

case class Config(helpRequest: Boolean = false, proxy: Option[Proxy] = None, login: Option[String] = None, password: Option[String] = None)
