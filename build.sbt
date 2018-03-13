name := "shapelessGuide"

version := "0.1"

scalaVersion := "2.11.8"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

inThisBuild(Seq(
  scalaOrganization := "org.typelevel",
  scalaVersion := "2.12.4-bin-typelevel-4"
))


libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.3"
)