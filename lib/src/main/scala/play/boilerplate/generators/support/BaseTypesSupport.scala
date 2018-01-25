package play.boilerplate.generators.support

import play.boilerplate.parser.model._

trait BaseTypesSupport {

  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  private lazy val OffsetDateTimeClass = definitions.getClass("java.time.OffsetDateTime")
  private lazy val LocalDateClass      = definitions.getClass("java.time.LocalDate")
  private lazy val UUIDClass           = definitions.getClass("java.util.UUID")
  private lazy val FileClass           = definitions.getClass("java.io.File")

  def getSimpleTypeSupport(definition: SimpleDefinition): TypeSupport = {
    definition match {
      case str: StringDefinition =>
        getStringSupport(str)
      case email: EmailDefinition =>
        getEmailSupport(email)
      case bool: BooleanDefinition =>
        getBooleanSupport(bool)
      case double: DoubleDefinition =>
        getDoubleSupport(double)
      case float: FloatDefinition =>
        getFloatSupport(float)
      case int: IntegerDefinition =>
        getIntegerSupport(int)
      case long: LongDefinition =>
        getLongSupport(long)
      case decimal: DecimalDefinition =>
        getDecimalSupport(decimal)
      case date: DateDefinition =>
        getDateSupport(date)
      case dt: DateTimeDefinition =>
        getDateTimeSupport(dt)
      case uuid: UUIDDefinition =>
        getUUIDSupport(uuid)
      case file: FileDefinition =>
        getFileSupport(file)
    }
  }

  def getStringSupport(str: StringDefinition): TypeSupport = {
    TypeSupport(StringClass, StringClass, Nil)
  }

  def getEmailSupport(str: EmailDefinition): TypeSupport = {
    TypeSupport(StringClass, StringClass, Nil)
  }

  def getBooleanSupport(bool: BooleanDefinition): TypeSupport = {
    TypeSupport(BooleanClass, BooleanClass, Nil)
  }

  def getDoubleSupport(double: DoubleDefinition): TypeSupport = {
    TypeSupport(DoubleClass, DoubleClass, Nil)
  }

  def getFloatSupport(float: FloatDefinition): TypeSupport = {
    TypeSupport(FloatClass, FloatClass, Nil)
  }

  def getIntegerSupport(int: IntegerDefinition): TypeSupport = {
    TypeSupport(IntClass, IntClass, Nil)
  }

  def getLongSupport(long: LongDefinition): TypeSupport = {
    TypeSupport(LongClass, LongClass, Nil)
  }

  def getDecimalSupport(decimal: DecimalDefinition): TypeSupport = {
    TypeSupport(BigDecimalClass, BigDecimalClass, Nil)
  }

  def getDateSupport(date: DateDefinition): TypeSupport = {
    TypeSupport(LocalDateClass, LocalDateClass, Nil)
  }

  def getDateTimeSupport(dt: DateTimeDefinition): TypeSupport = {
    TypeSupport(OffsetDateTimeClass, OffsetDateTimeClass, Nil)
  }

  def getUUIDSupport(uuid: UUIDDefinition): TypeSupport = {
    TypeSupport(UUIDClass, UUIDClass, Nil)
  }

  def getFileSupport(file: FileDefinition): TypeSupport = {
    TypeSupport(FileClass, FileClass, Nil)
  }

}
