name := "worksheetcells"
organization := "org.rcano"

scalaVersion := "3.3.4"

scalacOptions ++= Seq(
  "-Wunused:all",
  "-deprecation",
  "-unchecked",
  "-Yexplain-lowlevel",
  "-Xprint-inline",
  "-pagewidth",
  "120",
  "-Xmax-inlines",
  "1024",
)

Compile / packageDoc / publishArtifact := false

fork := true
outputStrategy := Some(StdoutOutput)

libraryDependencies ++= Seq(
  "com.github.pathikrit" %% "better-files" % "3.9.2",
  "com.lihaoyi" %% "pprint" % "0.9.0",

  // special byte manipulation library that lets us open up all modules, necessary for kryo and GWMS
  "org.burningwave" % "core" % "12.65.2",
  "com.twitter" %% "chill" % "0.10.0" cross CrossVersion.for3Use2_13,
  "org.scalameta" %% "munit" % "0.7.29" % Test
)

testFrameworks += new TestFramework("munit.Framework")
