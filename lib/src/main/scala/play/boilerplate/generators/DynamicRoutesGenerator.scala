package play.boilerplate.generators

final class DynamicRoutesGenerator extends RoutesGeneratorParser {
  override def generateFullClassName(className: String): String = {
    "@" + className
  }
}
