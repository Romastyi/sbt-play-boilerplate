package play.boilerplate.generators.support

import play.boilerplate.generators.GeneratorContext
import play.boilerplate.parser.model.{ComplexDefinition, SimpleDefinition}

trait CustomTypeSupport {

  def getComplexTypeSupport: GeneratorContext => CustomTypeSupport.ComplexTypeSupport

  def getSimpleTypeSupport: GeneratorContext => CustomTypeSupport.SimpleTypeSupport

}

object CustomTypeSupport {

  type ComplexTypeSupport = PartialFunction[(ComplexDefinition, DefinitionContext), TypeSupport]
  type SimpleTypeSupport  = PartialFunction[SimpleDefinition, TypeSupport]

  def empty: CustomTypeSupport = {
    new CustomTypeSupport {
      override val getComplexTypeSupport: GeneratorContext => CustomTypeSupport.ComplexTypeSupport = _ => Map.empty
      override val getSimpleTypeSupport: GeneratorContext => CustomTypeSupport.SimpleTypeSupport = _ => Map.empty
    }
  }

  def complex(f: GeneratorContext => CustomTypeSupport.ComplexTypeSupport): CustomTypeSupport = {
    new CustomTypeSupport {
      override val getComplexTypeSupport: GeneratorContext => CustomTypeSupport.ComplexTypeSupport = f
      override val getSimpleTypeSupport: GeneratorContext => CustomTypeSupport.SimpleTypeSupport = _ => Map.empty
    }
  }

  def simple(f: GeneratorContext => CustomTypeSupport.SimpleTypeSupport): CustomTypeSupport = {
    new CustomTypeSupport {
      override val getComplexTypeSupport: GeneratorContext => CustomTypeSupport.ComplexTypeSupport = _ => Map.empty
      override val getSimpleTypeSupport: GeneratorContext => CustomTypeSupport.SimpleTypeSupport = f
    }
  }

}
