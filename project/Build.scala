import sbt._
import Keys._
import sbt.Package._
import java.util.jar.Attributes.Name._

object BuildSettings {
  val buildOrganization = "onion-lang.org"
  val buildVersion = "1.0"
  val buildScalaVersion = "2.9.1"
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := buildOrganization,
    version      := buildVersion,
    scalaVersion := buildScalaVersion,
    shellPrompt  := ShellPrompt.buildShellPrompt
  )
}

// Shell prompt which show the current project, 
// git branch and build version
object ShellPrompt {
  object devnull extends ProcessLogger {
    def info (s: => String) {}
    def error (s: => String) { }
    def buffer[T] (f: => T): T = f
  }

  val buildShellPrompt = { 
    (state: State) => {
      val currProject = Project.extract (state).currentProject.id
      "%s:%s> ".format (
        currProject, BuildSettings.buildVersion
      )
    }
  }
}

object JassonBuild extends Build {
  import BuildSettings._
  lazy val jasson = Project(
    id = "jasson",
    base = file("."),
    settings = buildSettings
  )
}
