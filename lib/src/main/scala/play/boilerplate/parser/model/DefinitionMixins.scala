package play.boilerplate.parser.model

trait WithDefault[T] { this: Definition =>
  def default: Option[T]
}

trait WithMinMax[T] { this: Definition =>
  def minimum: Option[T]
  def maximum: Option[T]
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
