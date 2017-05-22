package play.boilerplate.generators

final class DynamicRoutesGenerator extends RoutesGenerator {
  override def generateFullClassName(className: String): String = {
    "@" + className
  }
}
