package play.boilerplate.generators

import java.io.File.{separator, separatorChar}

import play.boilerplate.generators.security.SecurityProvider

trait GeneratorContext {

  def fileName: String
  def basePackageName: String
  def codeProvidedPackage: String

  def modelPackageName: String

  def servicePackageName: String
  def serviceClassName: String

  def controllerPackageName: String
  def controllerClassName: String

  def currentPath: Seq[String]
  def addCurrentPath(path: String*): GeneratorContext

  def inModel: Boolean
  def setInModel(value: Boolean): GeneratorContext

  def inService: Boolean
  def setInService(value: Boolean): GeneratorContext

  def inController: Boolean
  def setInController(value: Boolean): GeneratorContext

  def enumGenerator: EnumerationGenerator
  def securityProvider: SecurityProvider

}

case class DefaultGeneratorContext(override val fileName: String,
                                   override val basePackageName: String,
                                   override val codeProvidedPackage: String,
                                   override val currentPath: Seq[String] = Nil,
                                   override val inModel: Boolean = false,
                                   override val inService: Boolean = false,
                                   override val inController: Boolean = false,
                                   override val enumGenerator: EnumerationGenerator = VanillaEnumerations,
                                   override val securityProvider: SecurityProvider = SecurityProvider.default
                                  ) extends GeneratorContext {

  def objectNameFromFileName(obj: String): String = {
    val sep = if (separatorChar == 92.toChar) "\\\\" else separator
    fileName.split(sep)
      .toList
      .last
      .replace(".yaml", "")
      .replace(".json", "")
      .capitalize + obj
  }

  override val modelPackageName: String = basePackageName + ".model"

  override val servicePackageName: String = basePackageName + ".service"
  override val serviceClassName: String = objectNameFromFileName("Service")

  override val controllerPackageName: String = basePackageName + ".controller"
  override val controllerClassName: String = objectNameFromFileName("Controller")

  override def addCurrentPath(path: String*): GeneratorContext = copy(currentPath = currentPath ++ path)

  override def setInModel(value: Boolean): GeneratorContext = copy(inModel = true)
  override def setInService(value: Boolean): GeneratorContext = copy(inService = true)
  override def setInController(value: Boolean): GeneratorContext = copy(inController = true)

}
