lazy val parallelism = project
  .in(file("."))
  .settings(
    name := "parallelism",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := "3.5.0",

    libraryDependencies += "org.scalameta" %% "munit" % "1.0.1" % Test,
    libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
  )

enablePlugins(JmhPlugin)
