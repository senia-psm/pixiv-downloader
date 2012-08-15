package senia.pixiv_downloader

import pixiv.{Url, Client}
import scalaz._
import Scalaz._

case class ParameterKey(name: String, description: String, symbol: Symbol, withParameter: Boolean)
case class MapMatcher[K, V](map: Map[K, V]) {
  def unapply(k: K): Option[V] = map.get(k)
}

object Test extends App {
  @annotation.tailrec def parseParams(args: List[String],
                                      keys: Map[String, ParameterKey],
                                      subresult: ValidationNEL[String, Map[Symbol, Option[String]]] = Map().success
                                     ): ValidationNEL[String, Map[Symbol, Option[String]]] = {
    val withParameter = MapMatcher(keys.filter{ case (k, v) => v.withParameter })
    val withoutParameter = MapMatcher(keys.filter{ case (k, v) => !v.withParameter })
    args match {
      case Nil => subresult
      case `withParameter`(k) :: l :: tail => parseParams(tail, keys, subresult.map { _ + (k.symbol -> l.some) })
      case `withParameter`(k) :: Nil =>
        (subresult |@| ("You should specify " + k.description + " after " + k.name + " key").failNel[Map[Symbol, String]])((c, r) => c)

//      case "--proxy" :: p :: tail => parseParams(tail, (subresult |@| Proxy.parse(p))((c, r) => c.copy(proxy = r.some)))
//      case "--proxy" :: Nil => (subresult |@| "You should specify proxy after --proxy key".failNel[Config])((c, r) => c)
      case `withoutParameter`(k) :: tail => parseParams(tail, keys, subresult.map { _ + (k.symbol -> None) })
      case head :: tail => parseParams(tail, keys, (subresult |@| ("Unsupported parameter: \"" + head + "\"").failNel[Config])((c, p) => c))
    }
  }

  val mandatoryKeys = Set(
    ParameterKey("-l", "pixiv login", 'login, withParameter = true),
    ParameterKey("--login", "pixiv login", 'login, withParameter = true),
    ParameterKey("-p", "pixiv password", 'password, withParameter = true),
    ParameterKey("--password", "pixiv password", 'password, withParameter = true)
  )

  val optionalKeys = Set(
    ParameterKey("--proxy", "proxy", 'proxy, withParameter = true),
    ParameterKey("-h", "help", 'help, withParameter = false),
    ParameterKey("--help", "help", 'help, withParameter = false)
  )

  def mkDescription(keys: Set[ParameterKey]) =
    keys.
      groupBy(k => k.symbol).
      map {
        case (s, pks) =>
          pks.map{_.name}.mkString("(", " | ", ")") +
          (if (pks.head.withParameter) " " + pks.head.description.replace(' ', '_') else "")
      }

  val usage =
    "pixiv_downloader " +
      mkDescription(mandatoryKeys).mkString(" ") +
      " " +
      mkDescription(optionalKeys).map{ "[" + _ + "]" }.mkString(" ")

//  val usage = """
//                |pixiv_downloader (-l | --login) login (-p | --password) password [(--help | -h)] [--proxy [protocol://][login:password@]proxy_name[:port]]
//                | """.stripMargin



  val configMap = parseParams(args.toList, (mandatoryKeys ++ optionalKeys).map{ k => k.name -> k}(collection.breakOut)) match {
    case Failure(f) =>
      println(f.list.mkString("\n"))
      println("Usage:")
      println(usage)
      sys.exit(1)
    case Success(c) => c
  }

  def getMandatory(keys: Map[Symbol, Option[String]], key: Symbol): ValidationNEL[String, String] =
    keys.get(key).flatMap( identity ).map{ _.successNel }.getOrElse( (key.toString().drop(1) + " is mandatory").failNel )

//  def getOptional(keys: Map[Symbol, Option[String]], key: Symbol): ValidationNEL[String, Option[String]] =
//    keys.get(key).map{ _.successNel }.getOrElse( None.successNel )

  val configResult =
    for {
      l <- getMandatory(configMap, 'login)
      p <- getMandatory(configMap, 'password)
      proxy <- configMap.get('proxy).flatMap(identity).map{Proxy.parse(_).map{_.some}}.getOrElse(None.successNel)
      h <- configMap.get('help).map{ _ => true }.getOrElse(false).successNel
    } yield Config(l, p, h, proxy)

  val config = configResult match {
    case Failure(f) =>
      println(f.list.mkString("\n"))
      println("Usage:")
      println(usage)
      sys.exit(1)
    case Success(c) => c
  }

  if (config.helpRequest) {
    println("Usage:")
    println(usage)
    sys.exit(0)
  }

  Client.withClient(config) { implicit client =>

    Client.getLoginPage
    Client.getLoginRedirect(config.login, config.password) match {
      case Success(Url.my_page) => println("my page redirect")
      case Success(x) => println("redirect to " + x)
      case Failure(l) => println("failed:\n" + l.list.mkString("\n"))
    }
  }


}




