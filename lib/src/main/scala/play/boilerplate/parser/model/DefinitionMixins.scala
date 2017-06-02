package play.boilerplate.parser.model

sealed trait Parameter {  this: Definition => }
trait BodyParameter { this: RefDefinition => }
trait HeaderParameter { this: Definition => }
trait PathParameter { this: Definition => }
trait QueryParameter { this: Definition => }

trait Property { this: Definition => }

trait Model { this: Definition => }

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