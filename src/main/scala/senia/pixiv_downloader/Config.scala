package senia.pixiv_downloader

import scalaz.Scalaz._

case class Config(login: String, password: String, proxy: Option[Proxy] = None)

case class ParameterKey(key: String, name: String, symbol: Symbol, withParameter: Boolean, description: Option[String] = None)
case class MapMatcher[K, V](map: Map[K, V]) {
  def unapply(k: K): Option[V] = map.get(k)
}

object Config{
  val mandatoryKeys = Set(
    ParameterKey("-l", "pixiv login", 'login, withParameter = true),
    ParameterKey("--login", "pixiv login", 'login, withParameter = true),
    ParameterKey("-p", "pixiv password", 'password, withParameter = true),
    ParameterKey("--password", "pixiv password", 'password, withParameter = true)
  )

  val optionalKeys = Set(
    ParameterKey("--proxy", "proxy", 'proxy, withParameter = true, "--proxy [protocol://][login:password@]proxy_name[:port]".some),
    ParameterKey("-h", "help", 'help, withParameter = false),
    ParameterKey("--help", "help", 'help, withParameter = false),
    ParameterKey("-d", "download", 'download, withParameter = true)
  )

  private def mkDescription(keys: Set[ParameterKey]) =
    keys.
      groupBy(k => k.symbol).
      map {case (s, pks) =>
      pks.head.description.getOrElse(
        (pks.map{_.key}.toList match {
          case head :: Nil => head
          case x => x.mkString("(", " | ", ")")
        }) +
          (if (pks.head.withParameter) " " + pks.head.name.replace(' ', '_') else "")
      )
    }

  val usage =
    "pixiv_downloader " +
      mkDescription(mandatoryKeys).mkString(" ") +
      " " +
      mkDescription(optionalKeys).map{ "[" + _ + "]" }.mkString(" ")

  def parseArguments[T <% Seq[String]](args: T): ValidationNEL[String, Map[Symbol, Option[String]]] = {
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
          (subresult |@| ("You should specify " + k.name + " after " + k.key + " key").failNel[Map[Symbol, String]])((c, r) => c)
        case `withoutParameter`(k) :: tail => parseParams(tail, keys, subresult.map { _ + (k.symbol -> None) })
        case head :: tail => parseParams(tail, keys, (subresult |@| ("Unsupported parameter: \"" + head + "\"").failNel[Config])((c, p) => c))
      }
    }

    parseParams(args.toList, (mandatoryKeys ++ optionalKeys).map{ k => k.key -> k}(collection.breakOut))
  }

  def apply(params: Map[Symbol, Option[String]]): ValidationNEL[String, Config] = {
    def getMandatory(keys: Map[Symbol, Option[String]], key: Symbol): ValidationNEL[String, String] =
      keys.get(key).flatMap( identity ).map{ _.successNel }.getOrElse( (key.toString().drop(1) + " is mandatory").failNel )

    for {
      l <- getMandatory(params, 'login)
      p <- getMandatory(params, 'password)
      proxy <- params.get('proxy).flatMap(identity).map{Proxy.parse(_).map{_.some}}.getOrElse(None.successNel)
    } yield Config(l, p, proxy)
  }
}
