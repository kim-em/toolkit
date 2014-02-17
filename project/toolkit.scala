import sbt._
import Keys._

object Toolkit extends Build {
  import BuildSettings._
  import Dependencies._

  lazy val root = Project(id = "toolkit",
    base = file("."),
    settings = buildSettings) aggregate (base, arithmetic, amazon, collections, algebra, functions, eval, wiki)

  lazy val base: Project = Project(id = "toolkit-base",
    base = file("base"),
    settings = buildSettings ++ Seq(libraryDependencies ++= Seq(commons.logging))) dependsOn ()

  lazy val arithmetic = Project(id = "toolkit-arithmetic",
    base = file("arithmetic"),
    settings = buildSettings) dependsOn ()

  lazy val amazon = Project(id = "toolkit-amazon",
    base = file("amazon"),
    settings = buildSettings ++ Seq(libraryDependencies ++= Seq(httpclient, jets3t, typica, commons.io))) dependsOn (base, collections)

  lazy val collections = Project(id = "toolkit-collections",
    base = file("collections"),
    settings = buildSettings) dependsOn (base, functions)

  lazy val algebra = Project(id = "toolkit-algebra",
    base = file("algebra"),
    settings = buildSettings ++ Seq(libraryDependencies ++= Seq(commons.math, apfloat, guava, findbugs))) dependsOn (base, arithmetic, amazon, functions, collections)

  lazy val functions = Project(id = "toolkit-functions",
    base = file("functions"),
    settings = buildSettings ++ Seq(libraryDependencies ++= Seq(guava)))

  lazy val eval = Project(id = "toolkit-eval",
    base = file("eval"),
    settings = buildSettings ++ Seq(dependsOnCompiler))

  lazy val wiki = Project(id = "toolkit-wiki",
    base = file("wiki"),
    settings = buildSettings ++ Seq(libraryDependencies ++= Seq(selenium.firefox, mysql, slick))) dependsOn (base)

}

object BuildSettings {
  import Resolvers._
  import Dependencies._

  val buildOrganization = "net.tqft"
  val buildVersion = "0.1.16-SNAPSHOT"
  val buildScalaVersion = "2.10.3"
  val buildCrossScalaVersions = Seq("2.10.3")

  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := buildOrganization,
    version := buildVersion,
    scalaVersion := buildScalaVersion,
    crossScalaVersions := buildCrossScalaVersions,
    publishTo := Some(Resolver.sftp("toolkit.tqft.net Maven repository", "tqft.net", "tqft.net/releases") as ("scottmorrison", new java.io.File("/Users/scott/.ssh/id_rsa"))),
    resolvers := sonatypeResolvers /* ++ SonatypeSettings.publishing */,
    libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
//    scalacOptions ++= Seq("-uniqid","-explaintypes"),
    scalacOptions ++= Seq("-optimise" /*,"-Yinline-warnings"*/),
    libraryDependencies ++= Seq(junit, slf4j))

  val dependsOnCompiler = libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) => deps :+ ("org.scala-lang" % "scala-compiler" % sv) }
}

object SonatypeSettings {
/*
  val publishing = Seq(
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { x => false },
    pomExtra := (
      <url>http://bitbucket.org/scottmorrison/toolkit</url>
      <licenses>
        <license>
          <name>CC-BY</name>
          <url>http://creativecommons.org/licenses/by/3.0/</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>https://bitbucket.org/scottmorrison/toolkit</url>
      </scm>
      <developers>
        <developer>
          <id>scott</id>
          <name>Scott Morrison</name>
          <url>http://tqft.net</url>
        </developer>
      </developers>),
    publishTo <<= version { v: String =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    })
    */
}

object Resolvers {
  val sonatypeResolvers = Seq(
    "Sonatype Nexus Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    "Sonatype Nexus Releases" at "https://oss.sonatype.org/content/repositories/releases")
}

object Dependencies {
        val junit = "junit" % "junit" % "4.11" % "test"
        val slf4j = "org.slf4j" % "slf4j-log4j12" % "1.6.1"
	val apfloat = "org.apfloat" % "apfloat" % "1.6.3"		// arbitrary precision integers and floats; much better than BigInt and BigDecimal
	object commons {
		val math = "org.apache.commons" % "commons-math" % "2.2"	// simplex algorithm
		val logging = "commons-logging" % "commons-logging" % "1.1.1"
		val io = "commons-io" % "commons-io" % "2.4"
	}
	val httpclient = "org.apache.httpcomponents" % "httpclient" % "4.3.2"
	val jets3t = "net.java.dev.jets3t" % "jets3t" % "0.9.0"
	val typica = "com.google.code.typica" % "typica" % "1.7.2"
	val guava = "com.google.guava" % "guava" % "16.0.1"
	val findbugs = "com.google.code.findbugs" % "jsr305" % "1.3.+"
	object selenium {
		val firefox = "org.seleniumhq.selenium" % "selenium-firefox-driver" % "2.39.0"
	}
	object lift {
		val util = "net.liftweb" %% "lift-util" % "2.6-M2"
	}
	val mysql = "mysql" % "mysql-connector-java" % "5.1.24"
	val slick = "com.typesafe.slick" %% "slick" % "2.0.0-RC1"
}

