package senia.pixiv_downloader

import pixiv.{Url, Client}
import scalaz._
import Scalaz._

object Test extends App {
  def showUsageAndExit(errors: Option[NonEmptyList[String]] = None) = {
    errors.foreach(l => Console.err.println(l.list.mkString("\n")))
    println("Usage:")
    println(Config.usage)
    sys.exit(1)
  }

  val configMap = Config.parseArguments(args) match {
    case Failure(f) => showUsageAndExit(f.some)
    case Success(c) => c
  }

  if (configMap.get('help).map{ _ => true }.getOrElse(false)) {
    showUsageAndExit()
  }

  val config = Config(configMap) match {
    case Failure(f) => showUsageAndExit(f.some)
    case Success(c) => c
  }

  Client.withClient(config) { implicit client =>

    Client.getLoginPage
    Client.getLoginRedirect(config.login, config.password) match {
      case Success(Url.my_page) => println("my page redirect")
      case Success(x) => println("redirect to " + x)
      case Failure(l) => println("failed:\n" + l.list.mkString("\n"))
    }
    println(Client.downloadFile("http://habrastorage.org/storage/habraeffect/f4/8e/f48e012bd2babdc5d90dd98fd7b880b6.png", "f48e012bd2babdc5d90dd98fd7b880b6.png"))
  }


}
