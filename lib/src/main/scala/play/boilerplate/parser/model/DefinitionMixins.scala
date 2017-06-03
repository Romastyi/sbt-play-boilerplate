package play.boilerplate.parser.model

sealed trait Parameter { self: RefDefinition =>
  def resolveWith(resolver: DefinitionResolver): Definition with Parameter
}
trait RefParameter extends Parameter { self: RefDefinition =>
  override def resolveWith(resolver: DefinitionResolver): Definition with Parameter = {
    new RefDefinition(name, ref.resolve(resolver)) with RefParameter
  }
}
trait BodyParameter extends Parameter { self: RefDefinition =>
  override def resolveWith(resolver: DefinitionResolver): Definition with Parameter = {
    new RefDefinition(name, ref.resolve(resolver)) with BodyParameter
  }
}
trait HeaderParameter extends Parameter { self: RefDefinition =>
  override def resolveWith(resolver: DefinitionResolver): Definition with Parameter = {
    new RefDefinition(name, ref.resolve(resolver)) with HeaderParameter
  }
}
trait PathParameter extends Parameter { self: RefDefinition =>
  override def resolveWith(resolver: DefinitionResolver): Definition with Parameter = {
    new RefDefinition(name, ref.resolve(resolver)) with PathParameter
  }
}
trait QueryParameter extends Parameter { self: RefDefinition =>
  override def resolveWith(resolver: DefinitionResolver): Definition with Parameter = {
    new RefDefinition(name, ref.resolve(resolver)) with QueryParameter
  }
}
trait FormParameter extends Parameter { self: RefDefinition =>
  override def resolveWith(resolver: DefinitionResolver): Definition with Parameter = {
    new RefDefinition(name, ref.resolve(resolver)) with FormParameter
  }
}

trait Property { self: RefDefinition =>
  def resolveWith(resolver: DefinitionResolver): Definition with Property = {
    new RefDefinition(name, ref.resolve(resolver)) with Property
  }
}

trait Model { self: RefDefinition =>
  def resolveWith(resolver: DefinitionResolver): Definition with Model = {
    new RefDefinition(name, ref.resolve(resolver)) with Model
  }
}

trait WithDefault[T] { this: Definition =>
  def default: Option[T]
}

trait WithFormat { this: Definition =>
  def format: Option[String]
}

trait WithMinMax[T] { this: Definition =>
  def min: Option[T]
  def max: Option[T]
}

trait WithMinMaxLength { this: Definition =>
  def minLength: Option[Int]
  def maxLength: Option[Int]
}

trait WithPattern { this: Definition =>
  def pattern: Option[String]
}

trait WithReadOnly { this: Definition =>
  def readOnly: Boolean
}