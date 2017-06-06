package play.boilerplate.generators

final class SingletonRoutesGenerator(implSuffix: String = "Impl") extends RoutesGeneratorParser {
  override def generateFullClassName(className: String): String = {
    className + implSuffix
  }
}
