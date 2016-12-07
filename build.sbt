name := "BAS"
version := "1.0"
scalaVersion := "2.11.0"

resolvers += "akka" at "http://repo.akka.io/snapshots"
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.4.14",
  "com.typesafe.akka" %% "akka-testkit" % "2.4.14"
)

PB.targets in Compile := Seq(
  scalapb.gen() -> (sourceManaged in Compile).value
)
