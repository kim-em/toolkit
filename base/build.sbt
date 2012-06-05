libraryDependencies ++= Seq(
	"junit" % "junit" % "4.8" % "test",
        "org.slf4j" % "slf4j-log4j12" % "1.6.1",
        "commons-logging" % "commons-logging" % "1.1.1"
)

// Scalatest
libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) =>
	val (scalatestVersion, scalatestScalaVersion) = sv match {
		case sv if sv.startsWith("2.9") => ("1.8.RC2", "2.9.0")
		case sv if sv.startsWith("2.10") => ("1.8-SNAPSHOT", "2.10.0-M3")
	}
    	deps :+ ("org.scalatest" % ("scalatest_" + scalatestScalaVersion) % scalatestVersion % "test" )
}

