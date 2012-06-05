libraryDependencies ++= Seq(
	"junit" % "junit" % "4.8" % "test",
        "org.apfloat" % "apfloat" % "1.6.3",		// arbitrary precision integers and floats; much better than BigInt and BigDecimal
        "org.apache.commons" % "commons-math" % "2.2"	// simplex algorithm
)

// Scalatest
libraryDependencies <<= (scalaVersion, libraryDependencies) { (sv, deps) =>
	val (scalatestVersion, scalatestScalaVersion) = sv match {
		case sv if sv.startsWith("2.9") => ("1.8.RC2", "2.9.0")
		case sv if sv.startsWith("2.10") => ("1.8-SNAPSHOT", "2.10.0-M3")
	}
    	deps :+ ("org.scalatest" % ("scalatest_" + scalatestScalaVersion) % scalatestVersion % "test" )
}

