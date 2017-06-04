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

  def getStringSupport(str: StringDefinition): TypeSupport = {
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
