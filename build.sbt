
scalaVersion := "2.11.6"

discoveredMainClasses in Compile ~= {_.sorted}

libraryDependencies ++= Seq(
  "com.github.nscala-time" %% "nscala-time" % "2.0.0",
  "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.3",
  "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3"
)

scalacOptions ++= Seq(
  "-deprecation"
)

// hprof settings

fork in run := true

javaOptions in run ++= Seq(
  "-agentlib:hprof=cpu=samples,depth=50",
  "-XX:+HeapDumpOnOutOfMemoryError",
  "-XX:MaxInlineSize=1"
)
