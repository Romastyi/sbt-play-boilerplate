package scaldi.play

import org.slf4j.ILoggerFactory
import play.api.{Configuration, Environment, LoggerConfigurator, OptionalDevContext}

// Fallback of missing components from original Play 2.7.x
class AdditionalBuildInComponents extends scaldi.Module {

  private def configureLoggerFactory(configuration: Configuration, environment: Environment): ILoggerFactory = {
    val loggerFactory: ILoggerFactory = LoggerConfigurator(environment.classLoader).map { lc =>
      lc.configure(environment, configuration, Map.empty)
      lc.loggerFactory
    }.getOrElse(org.slf4j.LoggerFactory.getILoggerFactory)

    if (shouldDisplayLoggerDeprecationMessage(configuration)) {
      val logger = loggerFactory.getLogger("application")
      logger.warn("Logger configuration in conf files is deprecated and has no effect. Use a logback configuration file instead.")
    }

    loggerFactory
  }

  private def shouldDisplayLoggerDeprecationMessage(appConfiguration: Configuration): Boolean = {
    import scala.collection.JavaConverters._
    import scala.collection.mutable

    val deprecatedValues = List("DEBUG", "WARN", "ERROR", "INFO", "TRACE", "OFF")

    // Recursively checks each key to see if it contains a deprecated value
    def hasDeprecatedValue(values: mutable.Map[String, AnyRef]): Boolean = {
      values.exists {
        case (_, value: String) if deprecatedValues.contains(value) =>
          true
        case (_, value: java.util.Map[_, _]) =>
          val v = value.asInstanceOf[java.util.Map[String, AnyRef]]
          hasDeprecatedValue(v.asScala)
        case _ =>
          false
      }
    }

    if (appConfiguration.underlying.hasPath("logger")) {
      appConfiguration.underlying.getAnyRef("logger") match {
        case value: String =>
          hasDeprecatedValue(mutable.Map("logger" -> value))
        case value: java.util.Map[_, _] =>
          val v = value.asInstanceOf[java.util.Map[String, AnyRef]]
          hasDeprecatedValue(v.asScala)
        case _ =>
          false
      }
    } else {
      false
    }
  }

  bind[ILoggerFactory] to configureLoggerFactory(inject[Configuration], inject[Environment])
  bind[OptionalDevContext] to new OptionalDevContext(None)
}
