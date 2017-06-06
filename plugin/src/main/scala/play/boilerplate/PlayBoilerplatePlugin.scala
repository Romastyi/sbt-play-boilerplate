package play.boilerplate

import play.boilerplate.generators._
import play.boilerplate.parser.backend.swagger.SwaggerBackend
import sbt.Keys._
import sbt.{Def, _}

object PlayBoilerplatePlugin extends AutoPlugin {

  object Keys {

    val generators: SettingKey[Seq[CodeGenerator]] = settingKey("generators")
    val generatorSettings: SettingKey[(String, String, String) => GeneratorSettings] = settingKey("generatorSettings")
    val generatorSourceDir: SettingKey[File] = settingKey("generatorSourceDir")
    val generatorDestPackage: SettingKey[String] = settingKey("generatorDestPackage")
    val generatorProvidedPackage: SettingKey[String] = settingKey("generatorProvidedPackage")

    val generateJson: SettingKey[Boolean] = settingKey("generateJson")
    val generateJsonCodeGenerator: SettingKey[CodeGenerator] = settingKey("generateJsonCodeGenerator")

    val generateModel: SettingKey[Boolean] = settingKey("generateModel")
    val generateModelCodeGenerator: SettingKey[CodeGenerator] = settingKey("generateModelCodeGenerator")

    val generateClient: SettingKey[Boolean] = settingKey("generateClient")
    val generateClientCodeGenerator: SettingKey[CodeGenerator] = settingKey("generateClientCodeGenerator")

    val generateServer: SettingKey[Boolean] = settingKey("generateServer")
    val generateServerCodeGenerator: SettingKey[CodeGenerator] = settingKey("generateServerCodeGenerator")

    val generateRoutes: SettingKey[Boolean] = settingKey("generateRoutes")
    val generateRoutesCodeGenerator: SettingKey[CodeGenerator] = settingKey("generateRoutesCodeGenerator")

    val generateServiceCodeGenerator: SettingKey[CodeGenerator] = settingKey("generateServiceCodeGenerator")

    val generatorsCodeGen: TaskKey[GeneratedFiles] = taskKey("generatorsCodeGen")
    val generatorsClean: TaskKey[Unit] = taskKey("generatorClean")

  }

  private def collectSwaggerFiles(sourceDir: File): Seq[File] = {
    if (sourceDir.exists() && sourceDir.isDirectory) {
      sourceDir.listFiles().filter(x => x.getName.endsWith(".json") || x.getName.endsWith(".yaml"))
    } else {
      println(s"Provided swagger source dir $sourceDir doesn't exists")
      Nil
    }
  }

  sealed trait GeneratedFile
  case class GeneratedSource(file: File) extends GeneratedFile
  case class GeneratedResource(file: File) extends GeneratedFile
  case class GeneratedFiles(sources: Seq[File], resources: Seq[File])

  private def writeFile(file: File, source: String): File = {
    IO.touch(file)
    IO.write(file, source)
    file
  }

  private def generateCodeFile(codeFile: CodeFile, sourcesDir: File, resourcesDir: File): GeneratedFile = {
    codeFile match {
      case source: SourceCodeFile =>
        GeneratedSource(writeFile(sourcesDir / source.fileName, source.source))
      case resource: ResourceFile =>
        GeneratedResource(writeFile(resourcesDir / resource.fileName, resource.source))
    }
  }

  private def generatorsCodeGenImpl(generators: Seq[CodeGenerator],
                                    settings: (String, String, String) => GeneratorSettings,
                                    sourceDir: File,
                                    sourceManagedDir: File,
                                    resourcesDir: File,
                                    destPackage: String,
                                    providedPackage: String): GeneratedFiles = {

    val generated = for {
      swaggerFile <- collectSwaggerFiles(sourceDir)
      swaggerFileName = swaggerFile.getAbsolutePath
      schema = SwaggerBackend.parseSchema(swaggerFileName).get
      context = GeneratorContext.initial(settings(swaggerFileName, destPackage, providedPackage))
      generator <- generators
      codeFile <- generator.generate(schema)(context)
    } yield generateCodeFile(codeFile, sourceManagedDir, resourcesDir)

    GeneratedFiles(
      sources = generated.collect {
        case GeneratedSource(file) => file
      },
      resources = generated.collect {
        case GeneratedResource(file) => file
      }
    )

  }

  private def generatorsCleanImpl(sourceManagedDir: File, destPackage: String): Unit = {
    IO.delete(sourceManagedDir / GeneratorUtils.classNameToPath(destPackage, "", ""))
  }

  override val requires: Plugins = plugins.JvmPlugin
  override def trigger: PluginTrigger = noTrigger

  override val projectSettings: Seq[Def.Setting[_]] = Seq(
    watchSources ++= Keys.generatorSourceDir.value.***.get,
    sourceGenerators in Compile += Keys.generatorsCodeGen.taskValue.map(_.sources),
    resourceGenerators in Compile += Keys.generatorsCodeGen.taskValue.map(_.resources),
    Keys.generators := {

      val jsonCodeGenerators = Seq(Keys.generateJsonCodeGenerator.value)
        .filter(_ => Keys.generateJson.value)
      val modelCodeGenerators = Seq(Keys.generateModelCodeGenerator.value)
        .filter(_ => Keys.generateModel.value)
      val clientCodeGenerators = Seq(Keys.generateClientCodeGenerator.value)
        .filter(_ => Keys.generateClient.value)
      val serverCodeGenerators = Seq(Keys.generateServerCodeGenerator.value)
        .filter(_ => Keys.generateServer.value)
      val serviceCodeGenerators = Seq(Keys.generateServiceCodeGenerator.value)
        .filter(_ => Keys.generateClient.value || Keys.generateServer.value)
      val routesCodeGenerators = Seq(Keys.generateRoutesCodeGenerator.value)
        .filter(_ => Keys.generateRoutes.value)

      jsonCodeGenerators ++
      modelCodeGenerators ++
      clientCodeGenerators ++
      serverCodeGenerators ++
      serviceCodeGenerators ++
      routesCodeGenerators

    },
    Keys.generatorSettings := { case (fileName, basePackageName, codeProvidedPackage) =>
      DefaultGeneratorSettings(fileName, basePackageName, codeProvidedPackage)
    },
    Keys.generatorSourceDir := (sourceDirectory in Compile).value / "swagger",
    Keys.generatorDestPackage := "test.api",
    Keys.generatorProvidedPackage := "",
    Keys.generateJson := true,
    Keys.generateJsonCodeGenerator := new JsonCodeGenerator(),
    Keys.generateModel := true,
    Keys.generateModelCodeGenerator := new ModelCodeGenerator(),
    Keys.generateClient := false,
    Keys.generateClientCodeGenerator := new ClientCodeGenerator(),
    Keys.generateServer := false,
    Keys.generateServerCodeGenerator := new ServerCodeGenerator(),
    Keys.generateRoutes := false,
    Keys.generateRoutesCodeGenerator := new DynamicRoutesCodeGenerator(),
    Keys.generateServiceCodeGenerator := new ServiceCodeGenerator(),
    Keys.generatorsCodeGen := {
      generatorsCodeGenImpl(
        Keys.generators.value,
        Keys.generatorSettings.value,
        Keys.generatorSourceDir.value,
        (sourceManaged in Compile).value,
        (resourceDirectory in Compile).value,
        Keys.generatorDestPackage.value,
        Keys.generatorProvidedPackage.value
      )
    },
    Keys.generatorsClean := {
      generatorsCleanImpl((sourceManaged in Compile).value, Keys.generatorDestPackage.value)
    }
  )

}
