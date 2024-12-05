name := "pbt"
scalaVersion := "3.5.0"
libraryDependencies += "org.scalameta" %% "munit" % "1.0.1" % Test
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.17.0"
scalacOptions ++= Seq("-deprecation", "-feature", "-language:fewerBraces", "-Xfatal-warnings")
