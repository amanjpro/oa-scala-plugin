import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._

object build extends Build {
  lazy val paradiseVersion = "2.0.0"
  lazy val root = Project(
    id = "root",
    base = file("."),
    aggregate = Seq(plugin, library, test)
  )

  lazy val sversion = "2.11.1"

  lazy val sharedSettings = Seq(
    scalaVersion := "2.11.1",
    resolvers += "Piuma's Repo" at 
          "http://inf.usi.ch/phd/sherwany/repos/piuma",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    organization := "ch.usi.inf.l3"

  )

  lazy val library = Project(
    id   = "library",
    base = file("library")
  ) settings (
    publishMavenStyle := true,
    credentials += Credentials(Path.userHome / ".ivy2" / ".pcredentials"),
    resourceDirectory in Compile <<= baseDirectory(_ / "resources"),
    publishTo := Some(Resolver.sftp("Object Algebra's Library Repo", 
                                      "euler.inf.unisi.ch",
                                      "public_html/repos/oa/library")),
    libraryDependencies ++= Seq("org.scala-lang" % "scala-reflect" % sversion,
        "org.scala-lang" % "scala-compiler" % sversion),
    addCompilerPlugin("org.scalamacros" % "paradise" % 
      paradiseVersion cross CrossVersion.full)

  ) settings (sharedSettings :_*)

  lazy val plugin =  Project(
    id   = "plugin",
    base = file("plugin")
  ) settings (
    publishMavenStyle := true,
    credentials += Credentials(Path.userHome / ".ivy2" / ".pcredentials"),
    resourceDirectory in Compile <<= baseDirectory(_ / "resources"),
    publishTo := Some(Resolver.sftp("Object Algebra's Plugin Repo", 
                                      "euler.inf.unisi.ch",
                                      "public_html/repos/oa/plugin")),
    resourceDirectory in Compile <<= baseDirectory(_ / "resources"),

    libraryDependencies ++= 
      Seq("ch.usi.inf.l3" %% "piuma" % "0.1-SNAPSHOT"),

    addCompilerPlugin("org.scalamacros" % "paradise" % 
      paradiseVersion cross CrossVersion.full)
  ) settings (
    artifact in (Compile, assembly) ~= { art =>
      art.copy(`classifier` = Some("assembly"))
    }
  ) settings (sharedSettings ++ assemblySettings ++ 
    addArtifact(artifact in (Compile, assembly), 
      assembly) : _*) dependsOn(library)
    
    

  // A regular module with the application code.
  lazy val test = Project (
    id   = "test",
    base = file("test")
  )  settings (
    autoCompilerPlugins := true,
    resolvers += "OA's Repo" at "http://inf.usi.ch/phd/sherwany/repos/oa",
    addCompilerPlugin(
      "ch.usi.inf.l3" %% "plugin" % 
      "0.1-SNAPSHOT" classifier "assembly" changing())
  ) settings (sharedSettings : _*) dependsOn(library)


  // val recompile = SettingKey[String]("republish", 
  //                                  """|Republishes the plugin, removes it from
  //                                     |the ivy2's cache, cleans main project 
  //                                     |and recompiles it""".stripMargin) 
  //
  // lazy val recompileTask = taskKey[Unit]("A task to recompile the project")
  // recompileTask := {
  //   sbt plugin/publish
  //   sbt main/clean
  //   sbt main/compile
  // }
}
