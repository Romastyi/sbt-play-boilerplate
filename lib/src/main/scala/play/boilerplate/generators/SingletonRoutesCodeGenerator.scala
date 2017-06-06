package play.boilerplate.generators

final class SingletonRoutesCodeGenerator(implSuffix: String = "Impl") extends RoutesCodeGenerator {
  override def generateFullClassName(className: String): String = {
    className + implSuffix
  }
}
