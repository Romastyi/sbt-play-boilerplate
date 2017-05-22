package play.boilerplate

import eu.unicredit.swagger.SwaggerCodegenPlugin._
import eu.unicredit.swagger.SwaggerCodegenPlugin.autoImport._
import eu.unicredit.swagger.generators.SyntaxString
import play.boilerplate.generators._
import sbt.Keys._
import sbt.{Def, _}

object PlayBoilerplatePlugin extends AutoPlugin {

  import injection.InjectionProvider
  import security.SecurityProvider

  object Keys {

    val swaggerInjectionProvider: SettingKey[InjectionProvider] = settingKey("swaggerDIProvider")
    val swaggerRoutesGenerator: SettingKey[RoutesGenerator] = settingKey("swaggerRoutesGenerator")
    val swaggerSecurityProvider: SettingKey[SecurityProvider] = settingKey("swaggerSecurityProvider")
    val swaggerServiceCodeTargetDir: SettingKey[File] = settingKey("swaggerServiceCodeTargetDir")
    val swaggerServiceCodeGen: TaskKey[Seq[File]] = taskKey("Generate swagger service boilerplate")

  }

  import Keys._

  private def swaggerServiceCodeGenImpl(targetDir: File,
                                        codegenPackage: String,
                                        sourcesDir: File,
                                        codeProvidedPackage: String,
                                        serviceGenerator: PlayServiceGenerator
                                       ): Seq[File] = {
    checkFileExistence(sourcesDir)
    IO delete targetDir

    val services: List[SyntaxString] =
      (for {
        file <- sourcesDir.listFiles()
        fName = file.getName
        fPath = file.getAbsolutePath
        if fName.endsWith(".json") || fName.endsWith(".yaml")
      } yield {
        serviceGenerator.generate(fPath, codegenPackage, codeProvidedPackage)
      }).flatten.toList

    val destDir = packageDir(targetDir, codegenPackage + ".service")

    services.foreach { ss =>
      IO write (destDir / (ss.name + ".scala"), ss.pre + "\n\n" + ss.code)
    }

    (destDir ** -DirectoryFilter).get
  }

  private val serviceDyn = Def.taskDyn {
    if (swaggerGenerateServer.value || swaggerGenerateClient.value)
      Def.task(swaggerServiceCodeGen.value)
    else
      Def.task(Seq.empty[File])
  }

  override val requires: Plugins = eu.unicredit.swagger.SwaggerCodegenPlugin
  override def trigger: PluginTrigger = noTrigger

  override val projectSettings = Seq(
    managedSources in Compile ++= serviceDyn.value,
    swaggerInjectionProvider := new InjectionProvider.DefaultInConstructor(),
    swaggerRoutesGenerator := new DynamicRoutesGenerator(),
    swaggerSecurityProvider := SecurityProvider.default,
    swaggerServiceCodeTargetDir := (sourceManaged in Compile).value / "swagger" / "service",
    swaggerServiceCodeGen := {
      swaggerServiceCodeGenImpl(
        targetDir = swaggerServiceCodeTargetDir.value.getAbsoluteFile,
        codegenPackage = swaggerCodeGenPackage.value,
        sourcesDir = swaggerSourcesDir.value.getAbsoluteFile,
        codeProvidedPackage = swaggerCodeProvidedPackage.value,
        serviceGenerator = new PlayServiceGenerator(swaggerSecurityProvider.value)
      )
    },
    swaggerClientCodeGenClass := new PlayClientGenerator(swaggerCodeProvidedPackage.value, swaggerSecurityProvider.value, swaggerInjectionProvider.value),
    swaggerServerCodeGenClass := new PlayServerGenerator(swaggerRoutesGenerator.value, swaggerSecurityProvider.value, swaggerInjectionProvider.value)
  )

}
