package play.boilerplate.generators.security

import io.swagger.models.{Operation => SwaggerOperation}
import play.boilerplate.parser.model.SecurityRequirement
import treehugger.forest._
import treehuggerDSL._

import scala.collection.JavaConversions._

trait SecurityProvider {

  import SecurityProvider._

  def controllerImports: Seq[Import]

  def controllerParents: Seq[Type]

  def controllerSelfTypes: Seq[Type]

  def serviceImports: Seq[Import]

  def getActionSecurity(security: Seq[SecurityRequirement]): ActionSecurity

}

object SecurityProvider {

  trait ActionSecurity {
    def actionMethod(parser: Tree): Tree
    def securityValues: Map[String, ValDef]
    def securityParams: Map[String, ValDef]
  }

  object WithoutSecurity extends ActionSecurity {
    override def actionMethod(parser: Tree): Tree = REF("Action.async") APPLY parser
    override val securityValues: Map[String, ValDef] = Map.empty
    override val securityParams: Map[String, ValDef] = Map.empty
  }

  def default: SecurityProvider = new SecurityProvider {
    override val controllerSelfTypes: Seq[Type] = Nil
    override val controllerImports: Seq[Import] = Nil
    override val controllerParents: Seq[Type] = Nil
    override val serviceImports: Seq[Import] = Nil
    override def getActionSecurity(security: Seq[SecurityRequirement]): ActionSecurity = WithoutSecurity
  }

  def getOperationSecurity(operation: SwaggerOperation): Option[Seq[SecurityRequirement]] = {
    Option(operation.getSecurity).map { security =>
      for {
        auth <- security
        (name, scopes) <- auth
      } yield SecurityRequirement(name, scopes.toIndexedSeq)
    }
  }

  def parseAction(operation: SwaggerOperation, provider: SecurityProvider): ActionSecurity = {
    getOperationSecurity(operation).map(provider.getActionSecurity).getOrElse(WithoutSecurity)
  }

}