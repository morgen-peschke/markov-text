enablePlugins(BuildInfoPlugin)

val silencerVersion = "1.3.3"
val catsVersion = "1.6.0"
val scalaTestVersion = "3.0.5"
val scalaCheckVersion = "1.14.0"
val shapelessVersion = "2.3.3"
val fs2Version = "1.0.4"
val circeVersion = "0.10.0"

val nameStr = "markov-text-gen"

name := nameStr
version := "0.0.1"
scalaOrganization := "org.typelevel"
scalaVersion := "2.12.4-bin-typelevel-4"

libraryDependencies ++= Seq(
  "co.fs2" %% "fs2-core" % fs2Version,
  "co.fs2" %% "fs2-io" % fs2Version,
  "com.chuusai" %% "shapeless" % shapelessVersion,
  "org.typelevel" %% "cats-core" % catsVersion,
  "com.monovore" %% "decline" % "0.5.0",
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
  "org.scalacheck" %% "scalacheck" % scalaCheckVersion % Test,
  "org.typelevel" %% "cats-laws" % catsVersion % Test,
  "org.typelevel" %% "cats-testkit" % catsVersion % Test
)
libraryDependencies ++= Seq(
  compilerPlugin("com.github.ghik" %% "silencer-plugin" % silencerVersion),
  "com.github.ghik" %% "silencer-lib" % silencerVersion % Provided
)

buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion)
buildInfoPackage := "peschke.markov.info"
buildInfoOptions += BuildInfoOption.BuildTime
buildInfoOptions += BuildInfoOption.ToJson

packGenerateWindowsBatFile := false
packMain := Map(nameStr -> "peschke.markov.driver.Main")