name := "pictura edifico"

version := "0.1.0"

scalaVersion := "2.11.1"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

resolvers ++= Seq("nparry.com" at "http://repository.nparry.com/releases",
                  "typesafe"   at "http://repo.typesafe.com/typesafe/releases",
                  "eaio.com"   at "http://eaio.com/maven2"
              )

libraryDependencies ++= Seq("org.specs2"      % "specs2_2.11" % "2.3.12" % "test")

//src/test/resources/itext

unmanagedBase := baseDirectory.value / "src/test/resources/itext"

testOptions in Test += Tests.Argument(TestFrameworks.Specs2, "junitxml", "console", "showtimes")


