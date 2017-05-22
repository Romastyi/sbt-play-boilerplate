package play.boilerplate.generators

final class SingletonRoutesGenerator(implSuffix: String = "Impl") extends RoutesGenerator {
  override def generateFullClassName(className: String): String = {
    className + implSuffix
  }
}
