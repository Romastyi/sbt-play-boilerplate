package play.boilerplate.generators

import play.boilerplate.parser.model.{Definition, Model}

final case class GeneratorContext private (settings: GeneratorSettings,
                                           currentPath: Seq[String],
                                           isModel: Boolean,
                                           currentModel: Option[Model],
                                           inService: Boolean,
                                           inClient: Boolean,
                                           modelsInOneFile: Boolean) {

  def addCurrentPath(path: String*): GeneratorContext = copy(currentPath = currentPath ++ path)
  def setIsModel(value: Boolean): GeneratorContext = copy(isModel = value)
  def setCurrentModel(value: Option[Model]): GeneratorContext = copy(currentModel = value)
  def setInService(value: Boolean): GeneratorContext = copy(inService = value)
  def setInClient(value: Boolean): GeneratorContext = copy(inClient = value)
  def setModelsInOneFile(value: Boolean): GeneratorContext = copy(modelsInOneFile = value)

  def isCurrentInterface(definition: Definition): Boolean = {
    this.currentModel.exists(m => m.baseDef == definition && m.isInterface)
  }

}

object GeneratorContext {

  def initial(settings: GeneratorSettings): GeneratorContext = {
    GeneratorContext(
      settings = settings,
      currentPath = Nil,
      isModel = false,
      currentModel = None,
      inService = false,
      inClient = false,
      modelsInOneFile = false
    )
  }

}