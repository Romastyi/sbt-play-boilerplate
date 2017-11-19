package play.boilerplate

import play.boilerplate.generators._
import play.boilerplate.parser.backend.swagger.SwaggerBackend
import sbt.Keys._
import sbt.{Def, _}

object PlayBoilerplatePlugin extends AutoPlugin {

  object Keys {

    trait GenSettings {
      def apply(fileName: String, basePackageName: String, codeProvidedPackage: String): GeneratorSettings
    }

    trait SchemasWatcher {
      protected val filter: NameFilter = "*.yaml" | "*.json"
      def schemas: Seq[File]
      def ++(other: SchemasWatcher): SchemasWatcher = SourcesWatcher(this.schemas ++ other.schemas)
    }

    case class SourcesWatcher(sources: Seq[File]) extends SchemasWatcher {
      override val schemas: Seq[File] = {
        (for (entry <- sources if filter accept entry) yield entry).distinct
      }
    }

    case class DirectoryWatcher(directory: File) extends SchemasWatcher {
      override def schemas: Seq[File] = (directory * filter).get
    }

    case class ClasspathJarsWatcher(dependencies: Classpath, toDir: File, jarFilter: NameFilter = "*.jar") extends SchemasWatcher {
      override def schemas: Seq[File] = {
        (for {
          entry <- dependencies if jarFilter.accept(entry.data)
          schema <- IO.unzip(entry.data, toDir, filter)
        } yield schema).distinct
      }
    }

    val generators: SettingKey[Seq[CodeGenerator]] = settingKey("generators")
    val generatorSettings: SettingKey[GenSettings] = settingKey("generatorSettings")
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

    val generateService: SettingKey[Boolean] = settingKey("generateService")
    val generateServiceCodeGenerator: SettingKey[CodeGenerator] = settingKey("generateServiceCodeGenerator")

    val generatorsSources: TaskKey[Seq[SchemasWatcher]] = taskKey("generatorWatchers")
    val generatorsCodeGen: TaskKey[GeneratedFiles] = taskKey("generatorsCodeGen")
    val generatorsClean: TaskKey[Unit] = taskKey("generatorClean")

  }

  def collectSchemas(watchers: Seq[Keys.SchemasWatcher]): Seq[File] = {
    watchers.reduceLeftOption(_ ++ _).map(_.schemas).getOrElse(Nil)
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
                                    genSettings: Keys.GenSettings,
                                    sourceManagedDir: File,
                                    resourcesDir: File,
                                    destPackage: String,
                                    providedPackage: String): Set[File] = {

    generatorsCleanImpl(sourceManagedDir, destPackage)

    for {
      swaggerFile <- swaggerFiles
      swaggerFileName = swaggerFile.getAbsolutePath
      schema = SwaggerBackend.parseSchema(swaggerFileName).get
      context = GeneratorContext.initial(genSettings(swaggerFileName, destPackage, providedPackage))
      generator <- generators
      codeFile <- generator.generate(schema)(context)
    } yield generateCodeFile(codeFile, sourceManagedDir, resourcesDir)

  }

  private def generatorsCleanImpl(sourceManagedDir: File, destPackage: String): Unit = {
    val files = sourceManagedDir / GeneratorUtils.classNameToPath(destPackage, "", "") ** ("*.scala" -- "routes*")
    IO.deleteFilesEmptyDirs(files.get)
  }

  override val requires: Plugins = plugins.JvmPlugin
  override def trigger: PluginTrigger = noTrigger

  override val projectSettings: Seq[Def.Setting[_]] = Seq(
    watchSources ++= collectSchemas(Keys.generatorsSources.value),
    sourceGenerators in Compile += Keys.generatorsCodeGen.taskValue.map(_.sources.toSeq),
    resourceGenerators in Compile += Keys.generatorsCodeGen.taskValue.map(_.resources.toSeq),
    Keys.generators := {

      val jsonCodeGenerators = Seq(Keys.generateJsonCodeGenerator.value)
        .filter(_ => Keys.generateJson.value)
      val modelCodeGenerators = Seq(Keys.generateModelCodeGenerator.value)
        .filter(_ => Keys.generateModel.value || Keys.generateService.value)
      val clientCodeGenerators = Seq(Keys.generateClientCodeGenerator.value)
        .filter(_ => Keys.generateClient.value)
      val serverCodeGenerators = Seq(Keys.generateServerCodeGenerator.value)
        .filter(_ => Keys.generateServer.value)
      val serviceCodeGenerators = Seq(Keys.generateServiceCodeGenerator.value)
        .filter(_ => Keys.generateClient.value || Keys.generateServer.value || Keys.generateService.value)
      val routesCodeGenerators = Seq(Keys.generateRoutesCodeGenerator.value)
        .filter(_ => Keys.generateRoutes.value)

      jsonCodeGenerators ++
      modelCodeGenerators ++
      clientCodeGenerators ++
      serverCodeGenerators ++
      serviceCodeGenerators ++
      routesCodeGenerators

    },
    Keys.generatorSettings := new Keys.GenSettings {
      def apply(fileName: String, basePackageName: String, codeProvidedPackage: String): GeneratorSettings =
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
    Keys.generateService := false,
    Keys.generateServiceCodeGenerator := new ServiceCodeGenerator(),
    Keys.generatorsCodeGen := {
      val cachedFiles = FileFunction.cached(
        (sourceManaged in Compile).value / ".sbt-play-boilerplate",
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
      val files = cachedFiles(collectSchemas(Keys.generatorsSources.value).toSet)
      GeneratedFiles(
        sources = files.flatMap(
          file => file.relativeTo((sourceManaged in Compile).value).map(_ => file)
        ),
        resources = files.flatMap(
          file => file.relativeTo((resourceDirectory in Compile).value).map(_ => file)
        )
      )
    },
    Keys.generatorsSources := {
      Seq(Keys.DirectoryWatcher(Keys.generatorSourceDir.value))
    },
    Keys.generatorsClean := {
      generatorsCleanImpl((sourceManaged in Compile).value, Keys.generatorDestPackage.value)
    }
  )

}
