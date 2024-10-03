name := "equational-reasoning"
scalaVersion := "3.5.0"
libraryDependencies += "ch.epfl.lara" %% "lisa" % "0.6" from "https://github.com/sankalpgambhir/lisa/raw/cs214/assembly/target/scala-3.3.1/lisa-assembly-0.6.jar"
libraryDependencies += "org.scalameta" %% "munit" % "1.0.1" % Test
libraryDependencies += "com.lihaoyi" %% "sourcecode" % "0.3.1"
scalacOptions ++= Seq("-deprecation", "-feature", "-language:fewerBraces", "-Xfatal-warnings")
run / fork := true
