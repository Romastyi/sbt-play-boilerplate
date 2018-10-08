package play.boilerplate

import play.boilerplate.generators.GeneratorUtils.MimeTypeSupport
import play.boilerplate.generators._
import play.boilerplate.generators.injection._
import play.boilerplate.generators.logger.LoggerProvider
import play.boilerplate.generators.security.SecurityProvider
import play.boilerplate.generators.support.CustomTypeSupport
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

    val enumGenerator: SettingKey[EnumerationGenerator] = settingKey("enumGenerator")
    val securityProvider: SettingKey[SecurityProvider] = settingKey("securityProvider")
    val injectionProvider: SettingKey[InjectionProvider] = settingKey("injectionProvider")
    val loggerProvider: SettingKey[LoggerProvider] = settingKey("loggerProvider")
    val customTypeSupport: SettingKey[CustomTypeSupport] = settingKey("customTypeSupport")
    val supportedMimeTypes: SettingKey[Map[String, MimeTypeSupport]] = settingKey("supportedMimeTypes")

    val generatorsSources: TaskKey[Seq[SchemasWatcher]] = taskKey("generatorsSources")
    val generatorsCodeGen: TaskKey[GeneratedFiles] = taskKey("generatorsCodeGen")
    val generatorsClean: TaskKey[Unit] = taskKey("generatorClean")

  }

  object Generators {

    val json: CodeGenerator = new JsonCodeGenerator()

    val model: CodeGenerator = new ModelCodeGenerator(inOneFile = false)

    val service: CodeGenerator = new ServiceCodeGenerator()

    val client: CodeGenerator = new ClientCodeGenerator()

    val controller: CodeGenerator = new ControllerCodeGenerator()

    val injectedController: CodeGenerator = InjectedControllerCodeGenerator

    def dynamicRoutesWithPrefix(prefix: String): CodeGenerator = DynamicRoutesCodeGenerator(prefix)

    val dynamicRoutes: CodeGenerator = dynamicRoutesWithPrefix("/")

    def injectedRoutesWithPrefix(prefix: String): CodeGenerator = InjectedRoutesCodeGenerator(prefix)

    val injectedRoutes: CodeGenerator = injectedRoutesWithPrefix("/")

    def staticRoutes(implSuffix: String, prefix: String = "/"): CodeGenerator = {
      SingletonRoutesCodeGenerator(implSuffix, prefix)
    }

    val sirdRoutes: CodeGenerator = sirdRoutesWithPrefix("/")

    def sirdRoutesWithPrefix(prefix: String): CodeGenerator = SirdRouterGenerator(prefix)

  }

  object Imports {

    import play.boilerplate.core.PluginVersion

    def component(id: String): ModuleID = PluginVersion.organization %% s"play-boilerplate-$id" % PluginVersion.current

    def api(PlayVersion: String): ModuleID = {
      CrossVersion partialVersion PlayVersion match {
        case Some((2, minor)) if minor >= 3 && minor <= 6 =>
          component(s"api-play2$minor")
        case _ =>
          component("api-play26")
      }
    }

    def scaldi(PlayVersion: String): ModuleID = {
      CrossVersion partialVersion PlayVersion match {
        case Some((2, 3)) => "org.scaldi" %% "scaldi-play-23" % "0.5.6"
        case Some((2, 4)) => "org.scaldi" %% "scaldi-play" % "0.5.13"
        case Some((2, 5)) => "org.scaldi" %% "scaldi-play" % "0.5.15"
        case Some((2, 6)) => "org.scaldi" %% "scaldi-play" % "0.5.17"
        case _ => "org.scaldi" %% "scaldi-play" % "0.5.17"
      }
    }

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
    // Default generators
    Keys.generators := Seq(Generators.json, Generators.model),
    // Generators code-gen settings
    Keys.enumGenerator      := VanillaEnumerations,
    Keys.securityProvider   := SecurityProvider.default,
    Keys.injectionProvider  := InjectionProvider.defaultInConstructor,
    Keys.loggerProvider     := LoggerProvider.defaultPlayLogger,
    Keys.customTypeSupport  := CustomTypeSupport.empty,
    Keys.supportedMimeTypes := Map.empty,
    Keys.generatorSettings  := new Keys.GenSettings {
      def apply(fileName: String, basePackageName: String, codeProvidedPackage: String) =
        DefaultGeneratorSettings(
          fileName,
          basePackageName,
          codeProvidedPackage,
          enumGenerator = Keys.enumGenerator.value,
          securityProvider = Keys.securityProvider.value,
          injectionProvider = Keys.injectionProvider.value,
          loggerProvider = Keys.loggerProvider.value,
          customTypeSupport = Keys.customTypeSupport.value,
          supportedMimeTypes = Keys.supportedMimeTypes.value
        )
    },
    // Generation sources and others
    Keys.generatorSourceDir := (sourceDirectory in Compile).value / "swagger",
    Keys.generatorDestPackage := "test.api",
    Keys.generatorProvidedPackage := "",
    // Generation tasks
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

  def ApiProject(name: String, dir: File)(PlayVersion: String): Project = Project(name, dir)
    .settings(
      Keys.generators := Seq(Generators.json, Generators.model, Generators.service, Generators.client),
      unmanagedResourceDirectories in Compile += Keys.generatorSourceDir.value,
      exportJars := true,
      libraryDependencies ++= Seq(
        "com.typesafe.play" %% "play-ws" % PlayVersion,
        Imports.api(PlayVersion)
      )
    )
    .enablePlugins(PlayBoilerplatePlugin)

  def ImplProject(name: String, dir: File, api: Project)(PlayVersion: String): Project = Project(name, dir)
    .settings(
      Keys.generators := Seq(Generators.controller, Generators.injectedRoutes),
      Keys.injectionProvider := GuiceInjectionProvider,
      Keys.generatorsSources += {
        val dependencies = (exportedProducts in Compile in api).value
        val toDirectory = (sourceManaged in Compile).value
        Keys.ClasspathJarsWatcher(dependencies, toDirectory)
      },
      libraryDependencies ++= Seq(
        "com.typesafe.play" %% "play" % PlayVersion
      )
    )
    .enablePlugins(PlayBoilerplatePlugin)
    .dependsOn(api)

}
