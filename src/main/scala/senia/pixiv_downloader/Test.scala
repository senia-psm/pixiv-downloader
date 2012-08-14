package senia.pixiv_downloader

import org.apache.http.HttpHost
import org.apache.http.client.{ResponseHandler, HttpClient}
import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.{BasicResponseHandler, DefaultHttpClient}
import org.apache.http.conn.params.ConnRoutePNames
import scala.util.control.Exception.allCatch
import org.apache.http.auth.AuthScope;
import org.apache.http.auth.UsernamePasswordCredentials;
import scalaz._
import Scalaz._
import scala.collection.JavaConverters._

import org.apache.http.Consts;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.cookie.Cookie;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;


case class Proxy(name: String, port: Option[Int] = None, protocol: Option[String] = None, login_password: Option[(String, String)] = None) {
  def actualPort = port.getOrElse(8080)
  lazy val host: HttpHost = new HttpHost(name, actualPort, protocol.getOrElse("http"))
}

object Proxy {
  class ProxyParser extends scala.util.parsing.combinator.JavaTokenParsers{
    override val skipWhitespace = false
    
    def proxy: Parser[Proxy] = opt(ident <~ "://") ~ opt( """[^:]+""".r ~ ":" ~ """[^@]+""".r <~ "@" ^^ { case l ~ _ ~ p => (l, p) } ) ~ """[^:]*""".r ~ opt(":" ~> """\d{1,6}""".r ^^ { _.toInt }) ^^ 
      { case protocol ~ login_password ~ name ~ port => Proxy(name, port, protocol, login_password)}
    
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

case class Config(helpRequest: Boolean = false, proxy: Option[Proxy] = None, login: Option[String] = None, password: Option[String] = None)

object Test extends App {
  @annotation.tailrec def parseParams(args: List[String], subresult: ValidationNEL[String, Config] = Config().success): ValidationNEL[String, Config] = args match {
    case Nil => subresult
    case ("--login" | "-l") :: l :: tail => parseParams(tail, subresult.map{ _.copy(login = l.some) } )
    case k @ ("--login" | "-l") :: Nil => (subresult |@| ("You should specify pixiv login after " + k + " key").failNel[Config])( (c, r) => c )
    case ("--password" | "-p") :: p :: tail => parseParams(tail, subresult.map{ _.copy(password = p.some) } )
    case k @ ("--password" | "-p") :: Nil => (subresult |@| ("You should specify pixiv password after " + k + " key").failNel[Config])( (c, r) => c )
    case "--proxy" :: p :: tail => parseParams(tail, (subresult |@| Proxy.parse(p))( (c, r) => c.copy(proxy = r.some) ))
    case "--proxy" :: Nil => (subresult |@| "You should specify proxy after --proxy key".failNel[Config])( (c, r) => c )
    case ("--help" | "-h") :: tail => parseParams(tail, subresult.map{ _.copy(helpRequest = true) } )
    case head :: tail => parseParams(tail, (subresult |@| ("Unsupported parameter: \"" + head + "\"").failNel[Config])( (c, p) => c))
  }

  val usage = """
    |pixiv_downloader (-l | --login) login (-p | --password) password [(--help | -h)] [--proxy [protocol://][login:password@]proxy_name[:port]]
    |""".stripMargin

  def withClient[T](config: Config)(f: DefaultHttpClient => T): T = {
    val client = new DefaultHttpClient()

    for {proxy <- config.proxy}
      client.getParams().setParameter(ConnRoutePNames.DEFAULT_PROXY, proxy.host)

    for {
      proxy <- config.proxy
      (login, password) <- proxy.login_password
    } client.getCredentialsProvider().setCredentials(
                    new AuthScope(proxy.name, proxy.actualPort),
                    new UsernamePasswordCredentials(login, password))

    allCatch.andFinally{ client.getConnectionManager().shutdown() }{ f(client) }
  }

  val config = parseParams(args.toList) match {
    case Failure(f) =>
      println(f.list.mkString("\n"))
      println("Usage:")
      println(usage)
      sys.exit(1)
    case Success(c) => c
  }
  
  val (pixiv_login, pixiv_password) = (config.login, config.password) match {
    case (Some(l), Some(p)) => (l, p)
    case _ =>
      println("Pixiv login and password are required.")
      println("Usage:")
      println(usage)
      sys.exit(1)
  }
  
  if (config.helpRequest) {
    println("Usage:")
    println(usage)
    sys.exit(0)
  }
  
  withClient(config){ implicit client =>
    def getLoginPage( implicit client: DefaultHttpClient) = {
      val httpget = new HttpGet("http://www.pixiv.net/login.php")
      val handler: ResponseHandler[String] = new BasicResponseHandler
      val response = client execute httpget
      val body = handler handleResponse response
      body
    }
    
    def login(implicit client: DefaultHttpClient) = {
      val httpost = new HttpPost("http://www.pixiv.net/login.php")//http://www.pixiv.net/login.php?pixiv_id=bloodyrat&pass=asdfqwer
      val nvps = List[NameValuePair](
        new BasicNameValuePair("pixiv_id", pixiv_login),
        new BasicNameValuePair("pass", pixiv_password),
        new BasicNameValuePair("mode", "login"),
        new BasicNameValuePair("skip", "1")
      )

      httpost.setEntity(new UrlEncodedFormEntity(nvps.asJava, Consts.UTF_8));

      val response = client.execute(httpost);
      response.getAllHeaders.foreach(println)
      val entity = response.getEntity();
      EntityUtils.consume(entity);
      entity
    }
	
    client.getCookieStore().getCookies().asScala.foreach(println)
    getLoginPage
    client.getCookieStore().getCookies().asScala.foreach(println)
    println(login)
    client.getCookieStore().getCookies().asScala.foreach(println)
  }
}
