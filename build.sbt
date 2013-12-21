
scalaVersion := "2.10.3"

discoveredMainClasses in Compile ~= {_.sorted}

libraryDependencies ++= Seq(
  "com.github.nscala-time" %% "nscala-time" % "0.2.0",
  "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.2",
  "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.2"
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
