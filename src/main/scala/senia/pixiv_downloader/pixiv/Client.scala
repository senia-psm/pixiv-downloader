package senia.pixiv_downloader.pixiv

import org.apache.http.impl.client.{BasicResponseHandler, DefaultHttpClient}
import org.apache.http.client.methods.{HttpPost, HttpGet}
import org.apache.http.{HttpStatus, HttpHeaders, Consts, NameValuePair}
import org.apache.http.message.BasicNameValuePair
import org.apache.http.client.entity.UrlEncodedFormEntity
import scalaz.Scalaz._
import org.apache.http.conn.params.ConnRoutePNames
import org.apache.http.auth.{UsernamePasswordCredentials, AuthScope}
import org.apache.http.params.CoreProtocolPNames
import util.control.Exception._
import senia.pixiv_downloader.Config
import scala.collection.JavaConverters._
import org.apache.http.client.HttpClient
import java.io.{BufferedOutputStream, BufferedInputStream}

object Client {
  def withClient[T](config: Config)(f: HttpClient => T): T = {
    val client = new DefaultHttpClient()

    for {proxy <- config.proxy}
      client.getParams.setParameter(ConnRoutePNames.DEFAULT_PROXY, proxy.host)

    for {
      proxy <- config.proxy
      (login, password) <- proxy.login_password
    } client.getCredentialsProvider.setCredentials(
      new AuthScope(proxy.name, proxy.actualPort),
      new UsernamePasswordCredentials(login, password))

    for {userAgent <- config.userAgent}
      client.getParams().setParameter(CoreProtocolPNames.USER_AGENT, userAgent);
    
    allCatch.andFinally {
      client.getConnectionManager.shutdown()
    } {
      f(client)
    }
  }

  def withGet[T](url: String)(f: HttpGet => T): T = {
    val get = new HttpGet(url)
    allCatch.andFinally(get.releaseConnection()){f(get)}
  }

  def withPost[T](url: String)(f: HttpPost => T): T = {
    val post = new HttpPost(url)
    allCatch.andFinally(post.releaseConnection()){f(post)}
  }

  def getLoginPage(implicit client: HttpClient) = getPage(Url.login)

  def getPage(url: String, referer: Option[String] = None)(implicit client: HttpClient) =
    withGet(url){ get =>
      for{r <- referer}
        get.addHeader(HttpHeaders.REFERER, r)
        
      val parserFactory = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
      val parser = parserFactory.newSAXParser()
      val adapter = new scala.xml.parsing.NoBindingFactoryAdapter
      
      val response = client execute get
      val source = new org.xml.sax.InputSource(response.getEntity().getContent()) 
      adapter.loadXML(source, parser)
    }
    
  def getLoginRedirect(login: String, password: String)(implicit client: HttpClient): ValidationNEL[String, String] =
    withPost(Url.login){ http_post =>
      val params = List[NameValuePair](
        new BasicNameValuePair("pixiv_id", login),
        new BasicNameValuePair("pass", password),
        new BasicNameValuePair("mode", "login"),
        new BasicNameValuePair("skip", "1")
      )
      http_post.setEntity(new UrlEncodedFormEntity(params.asJava, Consts.UTF_8))

      val response = client.execute(http_post)

      response.getStatusLine.getStatusCode match {
        case HttpStatus.SC_MOVED_TEMPORARILY =>
          response.getHeaders("location").toList match {
            case l :: Nil => l.getValue.successNel
            case head :: tail => (head :: tail).map { _.getValue }.mkString("Multiple locations: ", ",", ".").failNel
            case Nil => "No location found.".failNel
          }
        case x => ("Unsucceed status code: " + x).failNel
      }
    }

  def downloadImageById(id: String)(implicit client: HttpClient): ValidationNEL[String, Content] = {
    val page = getPage(Url.imgPageBig(id), Url.imgPageMedium(id).some)
    val imgs = page \\ "img" \ "@src"
    imgs.toList match {
      case h :: Nil => downloadFile(h.text, id + h.text.split("\\.").lastOption.map{"." + _}.getOrElse(""), Url.imgPageBig(id).some)
      case h :: t => "To many images".failNel
      case _ => "Cannot find image".failNel
    }
  }
  
  case class Content(length: Long, fileName: String, contentType: Option[String], encoding: Option[String])

  def downloadFile(sourceUrl: String, targetFile: String, referer: Option[String] = None)(implicit client: HttpClient): ValidationNEL[String, Content] = {
    import java.io.FileOutputStream
    def withFileOutputStream[T](fileName: String)(f: BufferedOutputStream => T): T = {
      val fos = new BufferedOutputStream(new FileOutputStream(fileName))
      allCatch.andFinally{fos.close()}{f(fos)}
    }

    withGet(sourceUrl) { get =>
      for{r <- referer}
        get.addHeader(HttpHeaders.REFERER, r)
      val response = client execute get
      response.getStatusLine.getStatusCode match {
        case HttpStatus.SC_OK =>
          Option(response.getEntity) match {
            case None => "Can't get entity".failNel
            case Some(entity) => withFileOutputStream(targetFile){ os =>
              entity.writeTo(os)
              Content(
                entity.getContentLength,
                targetFile,
                Option(entity.getContentType).map{_.getValue},
                Option(entity.getContentEncoding).map(_.getValue)).successNel
            }
          }
        case x => ("Unsucceed status code: " + x).failNel
      }
    }
  }
}
