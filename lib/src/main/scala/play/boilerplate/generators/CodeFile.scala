package play.boilerplate.generators

sealed trait CodeFile {
  def fileName: String
  def source: String
}

final case class SourceCodeFile(className: String, header: String, impl: String) extends CodeFile {
  override val fileName: String = className + ".scala"
  override val source: String = header + "\n" + impl
}
final case class ResourceFile(override val fileName: String, override val source: String) extends CodeFile
