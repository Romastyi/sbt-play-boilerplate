package play.boilerplate.parser.model

trait WithDefault[T] { this: Definition =>
  def default: Option[T]
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
