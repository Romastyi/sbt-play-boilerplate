package play.boilerplate.generators

final case class GeneratorContext private (settings: GeneratorSettings,
                                           currentPath: Seq[String],
                                           isModel: Boolean,
                                           inModel: Boolean,
                                           inService: Boolean,
                                           inClient: Boolean) {

  def addCurrentPath(path: String*): GeneratorContext = copy(currentPath = currentPath ++ path)
  def setIsModel(value: Boolean): GeneratorContext = copy(isModel = value)
  def setInModel(value: Boolean): GeneratorContext = copy(inModel = value)
  def setInService(value: Boolean): GeneratorContext = copy(inService = value)
  def setInClient(value: Boolean): GeneratorContext = copy(inClient = value)

}

object GeneratorContext {

  def initial(settings: GeneratorSettings): GeneratorContext = {
    GeneratorContext(
      settings = settings,
      currentPath = Nil,
      isModel = false,
      inModel = false,
      inService = false,
      inClient = false
    )
  }

}