package play.boilerplate.generators

import GeneratorUtils._

sealed trait CodeFile {
  def fileName: String
  def source: String
}

final case class SourceCodeFile(packageName: String, className: String, header: String, impl: String) extends CodeFile {
  override val fileName: String = classNameToPath(packageName, className, ".scala")
  override val source: String = header + "\n\n" + impl
}
final case class ResourceFile(override val fileName: String, override val source: String) extends CodeFile
