package play.boilerplate.generators

import GeneratorUtils._
import play.boilerplate.generators.injection.InjectionProvider
import play.boilerplate.generators.logger.LoggerProvider
import play.boilerplate.generators.security.SecurityProvider
import play.boilerplate.generators.support.CustomTypeSupport

abstract class GeneratorSettings(val fileName: String,
                                 val basePackageName: String,
                                 val codeProvidedPackage: String) {

  def modelPackageName: String
  def jsonPackageName: String
  def jsonObjectName: String
  def jsonImportPrefix: String

  def servicePackageName: String
  def serviceClassName: String

  def routesFileName: String
  def controllerPackageName: String
  def controllerClassName: String

  def clientPackageName: String
  def clientClassName: String

  def enumGenerator: EnumerationGenerator
  def securityProvider: SecurityProvider
  def injectionProvider: InjectionProvider
  def loggerProvider: LoggerProvider
  def customTypeSupport: CustomTypeSupport

}

case class DefaultGeneratorSettings(_fileName: String,
                                    _basePackageName: String,
                                    _codeProvidedPackage: String,
                                    override val enumGenerator: EnumerationGenerator = VanillaEnumerations,
                                    override val securityProvider: SecurityProvider = SecurityProvider.default,
                                    override val injectionProvider: InjectionProvider = InjectionProvider.defaultInConstructor,
                                    override val loggerProvider: LoggerProvider = LoggerProvider.defaultPlayLogger,
                                    override val customTypeSupport: CustomTypeSupport = CustomTypeSupport.empty)
  extends GeneratorSettings(_fileName, _basePackageName, _codeProvidedPackage) {

  override def modelPackageName: String = composeName(basePackageName, "model")
  override def jsonPackageName: String = composeName(modelPackageName, "json" )
  override def jsonObjectName: String = objectNameFromFileName(fileName, "Json")
  override val jsonImportPrefix: String = composeName(jsonPackageName, jsonObjectName)

  override val servicePackageName: String = composeName(basePackageName, "service")
  override val serviceClassName: String = objectNameFromFileName(fileName, "Service")

  override val routesFileName: String = composeName(stringToValidIdentifier(sanitizeFileName(fileName), skipNotValidChars = true), "routes")
  override val controllerPackageName: String = composeName(basePackageName, "controller")
  override val controllerClassName: String = objectNameFromFileName(fileName, "Controller")

  override val clientPackageName: String = composeName(basePackageName, "client")
  override val clientClassName: String = objectNameFromFileName(fileName, "Client")

}