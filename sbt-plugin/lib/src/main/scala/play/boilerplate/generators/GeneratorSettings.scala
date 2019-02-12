package play.boilerplate.generators

import play.boilerplate.generators.GeneratorUtils._
import play.boilerplate.generators.injection.InjectionProvider
import play.boilerplate.generators.logger.LoggerProvider
import play.boilerplate.generators.security.SecurityProvider
import play.boilerplate.generators.support.CustomTypeSupport

abstract class GeneratorSettings(val fileName: String,
                                 val basePackageName: String,
                                 val codeProvidedPackages: Seq[String]) {

  def modelPackageName: String
  def modelSingleFileName: String
  def jsonPackageName: String
  def jsonObjectName: String
  def jsonImportPrefix: String

  def serviceName: String
  def servicePackageName: String
  def serviceClassName: String

  def routesFileName: String
  def controllerPackageName: String
  def controllerClassName: String

  def clientPackageName: String
  def clientClassName: String

  def enumGenerator: EnumerationGenerator
  def securityProviders: Seq[SecurityProvider]
  def injectionProvider: InjectionProvider
  def loggerProvider: LoggerProvider
  def customTypeSupport: CustomTypeSupport
  def supportedMimeTypes: Map[String, MimeTypeSupport]

  def strictAcceptHeaderCheck: Boolean
  def useTraceId: Boolean
  def traceIdHeader: Option[String]
  final def effectiveTraceIdHeader: Option[String] = traceIdHeader.filter(_ => useTraceId)

}

case class DefaultGeneratorSettings(_fileName: String,
                                    _basePackageName: String,
                                    _codeProvidedPackages: Seq[String],
                                    override val enumGenerator: EnumerationGenerator = VanillaEnumerations,
                                    override val securityProviders: Seq[SecurityProvider] = Nil,
                                    override val injectionProvider: InjectionProvider = InjectionProvider.defaultInConstructor,
                                    override val loggerProvider: LoggerProvider = LoggerProvider.defaultPlayLogger,
                                    override val customTypeSupport: CustomTypeSupport = CustomTypeSupport.empty,
                                    override val supportedMimeTypes: Map[String, MimeTypeSupport] = Map.empty,
                                    override val strictAcceptHeaderCheck: Boolean = false,
                                    override val useTraceId: Boolean = false,
                                    override val traceIdHeader: Option[String] = None)
  extends GeneratorSettings(_fileName, _basePackageName, _codeProvidedPackages.distinct) {

  override def modelPackageName: String = composeName(basePackageName, "model")
  override def modelSingleFileName: String = objectNameFromFileName(fileName, "Models")
  override def jsonPackageName: String = composeName(modelPackageName, "json" )
  override def jsonObjectName: String = objectNameFromFileName(fileName, "Json")
  override val jsonImportPrefix: String = composeName(jsonPackageName, jsonObjectName)

  override def serviceName: String = sanitizeFileName(fileName)
  override val servicePackageName: String = composeName(basePackageName, "service")
  override val serviceClassName: String = objectNameFromFileName(fileName, "Service")

  override val routesFileName: String = composeName(stringToValidIdentifier(sanitizeFileName(fileName), skipNotValidChars = true), "routes")
  override val controllerPackageName: String = composeName(basePackageName, "controller")
  override val controllerClassName: String = objectNameFromFileName(fileName, "Controller")

  override val clientPackageName: String = composeName(basePackageName, "client")
  override val clientClassName: String = objectNameFromFileName(fileName, "Client")

}