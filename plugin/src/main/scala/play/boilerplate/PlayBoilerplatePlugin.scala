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

  private def collectSwaggerFiles(sourceDir: File): Set[File] = {
    if (sourceDir.exists() && sourceDir.isDirectory) {
      sourceDir.listFiles().filter(x => x.getName.endsWith(".json") || x.getName.endsWith(".yaml")).toSet
    } else {
      println(s"Provided swagger source dir $sourceDir doesn't exists")
      Set.empty
    }
  }

  case class GeneratedFiles(sources: Set[File], resources: Set[File]) {
    def all: Set[File] = sources ++ resources
  }

  private def writeFile(file: File, source: String): File = {
    IO.touch(file)
    IO.write(file, source)
    file
  }

  private def generateCodeFile(codeFile: CodeFile, sourcesDir: File, resourcesDir: File): File = {
    codeFile match {
      case source: SourceCodeFile =>
        writeFile(sourcesDir / source.fileName, source.source)
      case resource: ResourceFile =>
        writeFile(resourcesDir / resource.fileName, resource.source)
    }
  }

  private def generatorsCodeGenImpl(swaggerFiles: Set[File],
                                    generators: Seq[CodeGenerator],
                                    settings: (String, String, String) => GeneratorSettings,
                                    sourceManagedDir: File,
                                    resourcesDir: File,
                                    destPackage: String,
                                    providedPackage: String): Set[File] = {

    generatorsCleanImpl(sourceManagedDir, destPackage)

    for {
      swaggerFile <- swaggerFiles
      swaggerFileName = swaggerFile.getAbsolutePath
      schema = SwaggerBackend.parseSchema(swaggerFileName).get
      context = GeneratorContext.initial(settings(swaggerFileName, destPackage, providedPackage))
      generator <- generators
      codeFile <- generator.generate(schema)(context)
    } yield generateCodeFile(codeFile, sourceManagedDir, resourcesDir)

  }

  private def generatorsCleanImpl(sourceManagedDir: File, destPackage: String): Unit = {
    IO.delete(sourceManagedDir / GeneratorUtils.classNameToPath(destPackage, "", ""))
  }

  override val requires: Plugins = plugins.JvmPlugin
  override def trigger: PluginTrigger = noTrigger

  override val projectSettings: Seq[Def.Setting[_]] = Seq(
    watchSources ++= collectSwaggerFiles(Keys.generatorSourceDir.value).toSeq,
    sourceGenerators in Compile += Keys.generatorsCodeGen.taskValue.map(_.sources.toSeq),
    resourceGenerators in Compile += Keys.generatorsCodeGen.taskValue.map(_.resources.toSeq),
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
      val cachedFiles = FileFunction.cached(
        Keys.generatorSourceDir.value / ".sbt-play-boilerplate",
        FilesInfo.lastModified,
        FilesInfo.hash
      ) { files =>
        generatorsCodeGenImpl(
          files,
          Keys.generators.value,
          Keys.generatorSettings.value,
          (sourceManaged in Compile).value,
          (resourceDirectory in Compile).value,
          Keys.generatorDestPackage.value,
          Keys.generatorProvidedPackage.value
        )
      }
      val files = cachedFiles(collectSwaggerFiles(Keys.generatorSourceDir.value))
      GeneratedFiles(
        sources = files.flatMap(
          file => file.relativeTo((sourceManaged in Compile).value).map(_ => file)
        ),
        resources = files.flatMap(
          file => file.relativeTo((resourceDirectory in Compile).value).map(_ => file)
        )
      )
    },
    Keys.generatorsClean := {
      generatorsCleanImpl((sourceManaged in Compile).value, Keys.generatorDestPackage.value)
    }
  )

}
