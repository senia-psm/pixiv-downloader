package senia.pixiv_downloader.pixiv

object Url {
  lazy val base = "http://www.pixiv.net/"
  lazy val login = base + "login.php"
  lazy val my_page = base + "mypage.php"
  def imgPage(id: String, mode: String) = "http://www.pixiv.net/member_illust.php?mode=" + mode + "&illust_id=" + id
  def imgPageBig(id: String) = imgPage(id, "big")
  def imgPageMedium(id: String) = imgPage(id, "medium")
}
