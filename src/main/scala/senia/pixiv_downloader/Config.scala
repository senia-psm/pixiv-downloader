package senia.pixiv_downloader

case class Config(login: String, password: String, helpRequest: Boolean = false, proxy: Option[Proxy] = None)
