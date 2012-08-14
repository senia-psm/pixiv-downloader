name := "My Project"

version := "1.0"

scalaVersion := "2.9.1"

scalacOptions += "-deprecation"

libraryDependencies += "org.apache.httpcomponents" % "httpclient" % "4.2.1"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "com.typesafe.akka" % "akka-actor" % "2.0.2"

resolvers += "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "6.0.4"
