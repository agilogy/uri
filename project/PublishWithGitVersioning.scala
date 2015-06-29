import com.typesafe.sbt.GitVersioning
import com.typesafe.sbt.SbtGit.git
import sbt.Keys._
import sbt._

object PublishWithGitVersioning extends Build {
  import com.typesafe.sbt.GitVersioning
  import com.typesafe.sbt.SbtGit._

  val releaseVersion: SettingKey[Option[String]] = SettingKey[Option[String]]("releaseVersion")
  val isRelease = SettingKey[Boolean]("isRelease")

  override lazy val settings =
  // Add settings defined in build.sbt
    super.settings ++
      // Add GitVersioning.buildSettings to enable GitVersioning plugin
      GitVersioning.buildSettings ++
      // Add GitVersioning extra settings to configure versioning conventions
      Seq(
        git.gitTagToVersionNumber := { tag: String =>
          val ReleaseVersionPattern = "version-(.*)".r
          PartialFunction.condOpt(tag){
            case ReleaseVersionPattern(versionTag) => versionTag
          }
        },
        git.gitDescribedVersion := git.gitDescribedVersion.value.map{
          dv =>
            val res = git.gitTagToVersionNumber.value(dv).getOrElse(dv)
            if(git.gitUncommittedChanges.value) res + "-SNAPSHOT"
            else res
        },
        git.useGitDescribe := true,
        releaseVersion := {
          git.gitCurrentTags.value.find { tag =>
            git.gitTagToVersionNumber.value(tag).isDefined
          }
        },
        isRelease := releaseVersion.value.isDefined && !git.gitUncommittedChanges.value,
        publishTo := {
          val nexus = "http://spm.casaametller.net:50006/nexus-2.3.0-04/content/repositories/"
          if (isRelease.value) Some("releases"  at nexus + "releases")
          else Some("development" at nexus + "development")
        },
        publishMavenStyle := true,
        publishArtifact in Test := true
      )
}