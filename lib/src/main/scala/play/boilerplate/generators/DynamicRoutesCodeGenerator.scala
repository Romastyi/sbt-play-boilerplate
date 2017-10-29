package play.boilerplate.generators

final class DynamicRoutesCodeGenerator extends RoutesCodeGenerator {
  override def generateFullClassName(className: String): String = {
    "@" + className
  }
}
