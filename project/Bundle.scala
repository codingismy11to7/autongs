import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.{fastOptJS, fullOptJS}
import sbt.Keys._
import sbt._
import scalajsbundler.sbtplugin.ScalaJSBundlerPlugin.autoImport.webpack

object Bundle {

  private def doBundle(files: Seq[Attributed[File]], targetDir: File, version: String) = {
    val manifest =
      s"""{
         |  "name": "AutoNG",
         |  "manifest_version": 3,
         |  "host_permissions": [
         |    "https://ngspacecompany.freddecgames.com/",
         |    "https://micro500.github.io/NGSpaceCompany/"
         |  ],
         |  "content_scripts": [
         |    {
         |      "matches": [
         |        "https://ngspacecompany.freddecgames.com/",
         |        "https://micro500.github.io/NGSpaceCompany/"
         |      ],
         |      "js": [
         |        "autongs.js"
         |      ]
         |    }
         |  ],
         |  "description": "Automator for NG Space Company",
         |  "version": "$version"
         |}
         |""".stripMargin

    val extDir = targetDir / "extension"
    extDir.mkdirs()
    val mfFile = extDir / "manifest.json"
    IO.write(mfFile, manifest)
    val jsFile = extDir / "autongs.js"
    IO.copyFile(files.head.data, jsFile, CopyOptions().withOverwrite(true))

    Seq(mfFile, jsFile)
  }

  def createOptTask: Def.Initialize[Task[Seq[File]]] = Def.task {
    doBundle((Compile / fullOptJS / webpack).value, target.value, version.value)
  }

  def createFastTask: Def.Initialize[Task[Seq[File]]] = Def.task {
    doBundle((Compile / fastOptJS / webpack).value, target.value, version.value)
  }

}
