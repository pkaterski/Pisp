name := "final-project"
version := "0.1"

scalaVersion := "2.13.6"

libraryDependencies ++= Seq(
  // Enables logging if required by some library
  // You can use it via https://github.com/lightbend/scala-logging
  "ch.qos.logback" % "logback-classic" % "1.2.3",

  "org.scalatest" %% "scalatest" % "3.2.5" % Test,


  "org.typelevel" %% "cats-core" % "2.3.0"
)

assembly / assemblyMergeStrategy := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}
