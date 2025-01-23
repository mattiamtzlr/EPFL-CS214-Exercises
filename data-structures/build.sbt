name := "data-structures"
scalaVersion := "3.5.0"
libraryDependencies += "org.scalameta" %% "munit" % "1.0.1" % Test
scalacOptions ++= Seq("-deprecation", "-feature", "-language:fewerBraces", "-Xfatal-warnings")

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.17.0" % Test
libraryDependencies += "org.scalameta" %% "munit-scalacheck" % "1.0.0" % Test
