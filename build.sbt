name := "samplecode"

version := "1.0"

scalaVersion := "2.11.7"

val akkaVersion = "2.3.16"

ivyScala := ivyScala.value map { _.copy(overrideScalaVersion = true) }


libraryDependencies += "com.typesafe.akka" %% "akka-actor" % akkaVersion
libraryDependencies += "com.typesafe.akka" %% "akka-cluster" % akkaVersion
libraryDependencies += "com.typesafe.akka" % "akka-cluster-tools_2.11" % "2.4.17"
libraryDependencies += "com.typesafe.akka" % "akka-cluster-sharding_2.11" % "2.4.17"
libraryDependencies += "com.typesafe.akka" %% "akka-persistence" % akkaVersion
libraryDependencies +="com.typesafe.akka" %% "akka-multi-node-testkit" % akkaVersion
libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

    