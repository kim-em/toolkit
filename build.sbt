val junit = "junit" % "junit" % "4.12" % "test"
val slf4j = "org.slf4j" % "slf4j-log4j12" % "1.7.12"
val apfloat = "org.apfloat" % "apfloat" % "1.8.3"   // arbitrary precision integers and floats; much better than BigInt and BigDecimal
val commons_math = "org.apache.commons" % "commons-math3" % "3.5" // simplex algorithm
val commons_logging = "commons-logging" % "commons-logging" % "1.2"
val commons_io = "commons-io" % "commons-io" % "2.4"
val httpclient = "org.apache.httpcomponents" % "httpclient" % "4.3.2"
val jets3t = "net.java.dev.jets3t" % "jets3t" % "0.9.4"
val typica = "com.google.code.typica" % "typica" % "1.7.2"
val guava = "com.google.guava" % "guava" % "21.0"
val findbugs = "com.google.code.findbugs" % "jsr305" % "2.0.2"
val scopt = "com.github.scopt" %% "scopt" % "3.6.0"
val arm = "com.jsuereth" %% "scala-arm" % "1.4"
val selenium_firefox = "org.seleniumhq.selenium" % "selenium-firefox-driver" % "3.4.0"
val selenium_htmlunit = "org.seleniumhq.selenium" % "htmlunit-driver" % "2.27"
val lift_util = "net.liftweb" %% "lift-util" % "2.6"
val mysql = "mysql" % "mysql-connector-java" % "5.1.24"
val mapdb = "org.mapdb" % "mapdb" % "1.0.9"
val slick = "com.typesafe.slick" %% "slick" % "3.2.0"
val scalaz = "org.scalaz" %% "scalaz-core" % "7.1.2"
val spire = "org.spire-math" %% "spire" % "0.13.0"
val jblas = "org.jblas" % "jblas" % "1.2.4"
val omath_parser = "org.omath" %% "omath-parser" % "0.0.1"
val scalanlp_breeze = "org.scalanlp" %% "breeze" % "0.13.1"
val scalanlp_breezenatives = "org.scalanlp" %% "breeze-natives" % "0.13.1"
val scala_xml = "org.scala-lang.modules" %% "scala-xml" % "1.0.1"
val scala_parser = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
val scala_compiler = "org.scala-lang" % "scala-compiler" % "2.12.3"


lazy val buildSettings = Seq(
  organization := "net.tqft",
  version := "0.1.19-SNAPSHOT",
  scalaVersion := "2.12.3",
  publishConfiguration := publishConfiguration.value.withOverwrite(true),
  publishTo := Some(Resolver.sftp("tqft.net", "tqft.net", "tqft.net/releases") as ("scottmorrison", new java.io.File("/Users/scott/.ssh/id_rsa"))))

lazy val root = 
(project in file("."))
  .settings(name := "toolkit", buildSettings)
  .aggregate(
    base, 
    arithmetic,
    mathematica, 
    amazon, 
    collections, 
    permutations, 
    orderings, 
    algebra, 
    // `algebra-apfloat`,
    `algebra-polynomials`,
    `algebra-categories`,
    `algebra-matrices`,
    `algebra-groups`,
    `algebra-graphs`,
    `algebra-numberfields`,
    `algebra-mathematica`,
    // `algebra-magma`,
    `algebra-spiders`,
    // `algebra-fusion`,
    // `algebra-principalgraphs`,
    `algebra-enumeration`,
    // `algebra-experimental`, 
    functions, 
    eval, 
    // wiki
  )

lazy val base =
(project in file("base"))
   .settings(name := "toolkit-base", 
             buildSettings,
             libraryDependencies += commons_logging)

lazy val arithmetic =
(project in file("arithmetic"))
   .settings(name := "toolkit-arithmetic", 
             buildSettings)

lazy val functions =
(project in file("functions"))
   .settings(name := "toolkit-functions", 
             buildSettings,
             libraryDependencies ++= Seq(guava, findbugs))

lazy val collections =
(project in file("collections"))
   .settings(name := "toolkit-collections", 
             buildSettings)
   .dependsOn(base,functions)

lazy val mathematica =
(project in file("mathematica"))
   .settings(name := "toolkit-mathematica", 
             buildSettings,
             libraryDependencies ++= Seq(omath_parser, apfloat))

lazy val amazon =
(project in file("amazon"))
   .settings(name := "toolkit-amazon", 
             buildSettings,
             libraryDependencies ++= Seq(httpclient, jets3t, typica, commons_io))
   .dependsOn(base,collections)

lazy val orderings =
(project in file("orderings"))
   .settings(name := "toolkit-orderings", 
             buildSettings)

lazy val permutations =
(project in file("permutations"))
   .settings(name := "toolkit-permutations", 
             buildSettings)
   .dependsOn(orderings)

lazy val algebra =
(project in file("algebra"))
   .settings(name := "toolkit-algebra", 
             buildSettings)
   .dependsOn(arithmetic)

lazy val `algebra-apfloat` =
(project in file("algebra-apfloat"))
   .settings(name := "toolkit-algebra-apfloat", 
             buildSettings,
             libraryDependencies ++= Seq(apfloat))
   .dependsOn(algebra)

lazy val `algebra-polynomials` =
(project in file("algebra-polynomials"))
   .settings(name := "toolkit-algebra-polynomials", 
             buildSettings)
   .dependsOn(orderings, algebra, collections)

lazy val `algebra-categories` =
(project in file("algebra-categories"))
   .settings(name := "toolkit-algebra-categories", 
             buildSettings)
   .dependsOn(algebra, `algebra-polynomials`)

lazy val `algebra-matrices` =
(project in file("algebra-matrices"))
   .settings(name := "toolkit-algebra-matrices", 
             buildSettings)
   .dependsOn(collections, permutations, algebra, `algebra-polynomials`, `algebra-categories`)

lazy val `algebra-numberfields` =
(project in file("algebra-numberfields"))
   .settings(name := "toolkit-algebra-numberfields", 
             buildSettings)
   .dependsOn(`algebra-polynomials`, `algebra-matrices`)

lazy val `algebra-groups` =
(project in file("algebra-groups"))
   .settings(name := "toolkit-algebra-groups", 
             buildSettings)
   .dependsOn(`algebra-numberfields`)

lazy val `algebra-graphs` =
(project in file("algebra-graphs"))
   .settings(name := "toolkit-algebra-graphs", 
             buildSettings)
   .dependsOn(`algebra-groups`)

lazy val `algebra-mathematica` =
(project in file("algebra-mathematica"))
   .settings(name := "toolkit-algebra-mathematica", 
             buildSettings)
   .dependsOn(mathematica, `algebra-polynomials`)

lazy val `algebra-enumeration` =
(project in file("algebra-enumeration"))
   .settings(name := "toolkit-algebra-enumeration", 
             buildSettings)
   .dependsOn(arithmetic, `algebra-graphs`)

lazy val `algebra-spiders` =
(project in file("algebra-spiders"))
   .settings(name := "toolkit-algebra-spiders", 
             buildSettings,
             libraryDependencies ++= Seq(commons_io, scala_parser, mapdb, scalanlp_breeze, scalanlp_breezenatives))
   .dependsOn(mathematica, amazon, `algebra-mathematica`, `algebra-numberfields`, `algebra-apfloat`, `algebra-enumeration`)

lazy val `eval` =
(project in file("eval"))
   .settings(name := "toolkit-eval", 
             buildSettings,
             libraryDependencies += scala_compiler)

  // lazy val `algebra-magma` = Project(id = "toolkit-algebra-magma",
  //   base = file("algebra-magma"),
  //   settings = buildSettings ++ Seq(libraryDependencies ++= Seq())) dependsOn (`algebra-groups`)



  // lazy val `algebra-fusion` = Project(id = "toolkit-algebra-fusion",
  //     base = file("algebra-fusion"),
  //     settings = buildSettings ++ Seq(libraryDependencies ++= Seq(scopt, jblas))) dependsOn (functions, collections, algebra, `algebra-graphs`, `algebra-enumeration`, `algebra-magma`)

  // lazy val `algebra-bugs` = Project(id = "toolkit-algebra-bugs",
  //   base = file("algebra-bugs"),
  //   settings = buildSettings ++ Seq(libraryDependencies ++= Seq())) dependsOn (mathematica, `algebra-mathematica`)


  // lazy val `algebra-principalgraphs` = Project(id = "toolkit-algebra-principalgraphs",
  //   base = file("algebra-principalgraphs"),
  //   settings = buildSettings ++ Seq(libraryDependencies ++= Seq())) dependsOn (`algebra-enumeration`)

  // lazy val `algebra-experimental` = Project(id = "toolkit-algebra-experimental",
  //   base = file("algebra-experimental"),
  //   settings = buildSettings ++ Seq(libraryDependencies ++= Seq(commons.math, apfloat, guava, findbugs))) dependsOn (amazon, functions, collections, algebra, `algebra-categories`, `algebra-polynomials`, `algebra-groups`, `algebra-graphs`, `algebra-matrices`, `algebra-numberfields`, `algebra-apfloat`, `algebra-enumeration`)

  // lazy val eval = Project(id = "toolkit-eval",
  //   base = file("eval"),
  //   settings = buildSettings ++ Seq(dependsOnCompiler))

  // lazy val wiki = Project(id = "toolkit-wiki",
  //   base = file("wiki"),
  //   settings = buildSettings ++ 
  //     proguardSettings ++ 
  //     Seq(
  //       retrieveManaged := true,
  //       javaOptions in (Proguard, ProguardKeys.proguard) := Seq("-Xmx2G"), 
  //       ProguardKeys.options in Proguard ++= Seq("-dontnote", "-dontwarn", "-ignorewarnings"),
  //       ProguardKeys.options in Proguard += ProguardOptions.keepMain("net.tqft.toolkit.wiki.Wiki$")
  //     ) ++ 
  //     com.github.retronym.SbtOneJar.oneJarSettings ++ 
  //     Seq(libraryDependencies ++= Seq(selenium.firefox, selenium.htmlunit, mysql, slick))) dependsOn (base)


// object BuildSettings {
//   import Resolvers._
//   import Dependencies._

//   val buildOrganization = "net.tqft"
//   val buildVersion = "0.1.18-SNAPSHOT"
//   val buildScalaVersion = "2.12.3"

//   val buildSettings = Defaults.defaultSettings ++ Seq(
//     organization := buildOrganization,
//     version := buildVersion,
//     scalaVersion := buildScalaVersion,
//     scalacOptions += "-deprecation",
//     publishTo := {
//         val key = new java.io.File(Path.userHome.absolutePath + "/.ssh/id_rsa")
//         if(key.exists) {
//           Some(Resolver.sftp("toolkit.tqft.net Maven repository", "tqft.net", "tqft.net/releases") as ("scottmorrison", key))
//         } else {
//           None
//         }
//     },
//     resolvers := sonatypeResolvers ++ tqftResolvers /* ++ SonatypeSettings.publishing */,
//     libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test",
//     //    scalacOptions ++= Seq("-uniqid","-explaintypes"),
//     //    scalacOptions ++= Seq("-optimise" /*,"-Yinline-warnings"*/),
//     libraryDependencies ++= Seq(junit, slf4j),
//     exportJars := true,
//     fork := true,
//     javaOptions in run += "-Xmx8G",
//     connectInput in run := true,
//     baseDirectory in run := file("."),
//     EclipseKeys.withSource := true,
//     EclipseKeys.eclipseOutput := Some(".target")
//   )

//   val dependsOnCompiler = libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) => deps :+ ("org.scala-lang" % "scala-compiler" % sv) }
// }



// object Resolvers {
//   val sonatypeResolvers = Seq(
//     "Sonatype Nexus Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
//     "Sonatype Nexus Releases" at "https://oss.sonatype.org/content/repositories/releases",
//     "Maven Central" at "http://central.maven.org/maven2/")
//   val tqftResolvers = Seq("tqft.net Maven repository" at "https://tqft.net/releases"	
//   )
// }


