import bintray.Keys._

organization := "com.agilogy"

name := "uris"

scalaVersion := "2.11.7"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"

// --> Linters

// See tinyurl.com/sd15lint

// https://tpolecat.github.io/2014/04/11/scalac-flags.html
scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",       // yes, this is 2 args
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture"
)

// Execute static analysis via `lint:compile`
val LintTarget = config("lint").extend(Compile)

inConfig(LintTarget) {

  Defaults.compileSettings ++
    Seq(
      sources in LintTarget := {
        val lintSources = (sources in LintTarget).value
        lintSources ++ (sources in Compile).value
      },
      scalacOptions in LintTarget ++= Seq(
        "-Xfatal-warnings",
        "-Ywarn-unused-import",
        "-Ywarn-dead-code",
        "-P:linter:disable:PreferIfToBooleanMatch"
      ),
      wartremoverErrors ++= Warts.allBut(Wart.DefaultArguments, Wart.MutableDataStructures)
    )
}

scalacOptions in Compile := (scalacOptions in Compile).value filterNot { switch =>
  switch.startsWith("-P:wartremover:") ||
    "^-Xplugin:.*/org[.]brianmckenna/.*wartremover.*[.]jar$".r.pattern.matcher(switch).find ||
    switch.startsWith("-P:linter:") ||
    "^-Xplugin:.*/com[.]foursquare[.]lint/.*linter.*[.]jar$".r.pattern.matcher(switch).find
}

resolvers += "Linter Repository" at "https://hairyfotr.github.io/linteRepo/releases"

addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.12")

scalastyleFailOnError := true

// <-- Linters

// Reformat at every compile.
// See https://github.com/sbt/sbt-scalariform
scalariformSettings

ScoverageSbtPlugin.ScoverageKeys.coverageExcludedPackages := "<empty>"

// --> bintray

seq(bintrayPublishSettings:_*)

repository in bintray := "scala"

bintrayOrganization in bintray := Some("agilogy")

packageLabels in bintray := Seq("scala")

licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html"))

// <-- bintray

enablePlugins(GitVersioning)

git.useGitDescribe := true

publishMavenStyle := isSnapshot.value

publishTo := {
  val nexus = "http://188.166.95.201:8081/content/repositories/snapshots"
  if (isSnapshot.value) Some("snapshots"  at nexus)
  else publishTo.value
}