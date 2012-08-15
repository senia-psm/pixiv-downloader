package senia.pixiv_downloader

import org.apache.http.HttpHost
import scalaz.Scalaz._

case class Proxy(name: String,
                 port: Option[Int] = None,
                 protocol: Option[String] = None,
                 login_password: Option[(String, String)] = None) {
  def actualPort = port.getOrElse(8080)
  lazy val host: HttpHost = new HttpHost(name, actualPort, protocol.getOrElse("http"))
}

object Proxy {

  class ProxyParser extends scala.util.parsing.combinator.JavaTokenParsers {
    override val skipWhitespace = false

    def proxy: Parser[Proxy] =
      opt(ident <~ "://") ~
        opt( """[^:]+""".r ~ ":" ~ """[^@]+""".r <~ "@" ^^ { case l ~ _ ~ p => (l, p) }) ~
        """[^:]*""".r ~ opt(":" ~> """\d{1,6}""".r ^^ { _.toInt }) ^^
        { case protocol ~ login_password ~ name ~ port => Proxy(name, port, protocol, login_password) }

    def apply(s: String) = parseAll(proxy, s)
  }


  def parse(s: String): ValidationNEL[String, Proxy] = {
    val p = new ProxyParser()
    p(s) match {
      case p.Success(r, _) => r.successNel
      case x => ("Can't parse proxy \"" + s + "\":\n" + x).failNel
    }
  }
}