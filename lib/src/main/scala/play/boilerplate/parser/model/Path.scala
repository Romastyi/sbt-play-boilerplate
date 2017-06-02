package play.boilerplate.parser.model

case class Path(pathUrl: String, pathParts: Iterable[PathPart], operations: Map[HttpMethod.Value, Operation])

sealed trait PathPart
final case class StaticPart(s: String) extends PathPart
final case class ParamPart(name: String) extends PathPart
