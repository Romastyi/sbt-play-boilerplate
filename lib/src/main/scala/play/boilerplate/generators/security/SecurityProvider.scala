package play.boilerplate.generators.security

import io.swagger.models.{Operation => SwaggerOperation}
import play.boilerplate.generators.injection.InjectionProvider.Dependency
import play.boilerplate.parser.model.SecurityRequirement
import treehugger.forest._
import treehuggerDSL._

import scala.collection.JavaConverters._

trait SecurityProvider {

  import SecurityProvider._

  def controllerImports: Seq[Import]

  def controllerParents: Seq[Type]

  def controllerSelfTypes: Seq[Type]

  def controllerDependencies: Seq[Dependency]

  def serviceImports: Seq[Import]

  def getActionSecurity(security: Seq[SecurityRequirement]): ActionSecurity

}

object SecurityProvider {

  trait ActionSecurity {
    def actionMethod(parser: Tree): Tree
    def securityValues: Map[String, ValDef]
    def securityParams: Map[String, Type]
    def securityDocs: Map[String, String]
    def securityParamsDef: Iterable[ValDef] = securityParams.map {
      case (name, tpe) => PARAM(name, tpe).empty
    }
  }

  object WithoutSecurity extends ActionSecurity {
    override def actionMethod(parser: Tree): Tree = REF("Action.async") APPLY parser
    override val securityValues: Map[String, ValDef] = Map.empty
    override val securityParams: Map[String, Type] = Map.empty
    override val securityDocs: Map[String, String] = Map.empty
  }

  def default: SecurityProvider = new SecurityProvider {
    override val controllerSelfTypes: Seq[Type] = Nil
    override val controllerImports: Seq[Import] = Nil
    override val controllerParents: Seq[Type] = Nil
    override val controllerDependencies: Seq[Dependency] = Nil
    override val serviceImports: Seq[Import] = Nil
    override def getActionSecurity(security: Seq[SecurityRequirement]): ActionSecurity = WithoutSecurity
  }

  def getOperationSecurity(operation: SwaggerOperation): Option[Seq[SecurityRequirement]] = {
    Option(operation.getSecurity).map { security =>
      for {
        auth <- security.asScala
        (name, scopes) <- auth.asScala
      } yield SecurityRequirement(name, scopes.asScala.toIndexedSeq)
    }
  }

  def parseAction(operation: SwaggerOperation, provider: SecurityProvider): ActionSecurity = {
    getOperationSecurity(operation).map(provider.getActionSecurity).getOrElse(WithoutSecurity)
  }

  case class SecurityScope(s: String) {
    val scope: String = s.split(':').head
    val values: Seq[String] = for (i <- s.split(':').tail; v <- i.split(',')) yield v
  }

  abstract class DefaultSecurity(securitySchema: String) extends SecurityProvider {

    def composeActionSecurity(scopes: Seq[SecurityScope]): ActionSecurity

    override def getActionSecurity(security: Seq[SecurityRequirement]): ActionSecurity = {
      security.find(_.schemaName == securitySchema) match {
        case Some(SecurityRequirement(_, scopes)) =>
          composeActionSecurity(scopes.toIndexedSeq.map(SecurityScope.apply))
        case None =>
          WithoutSecurity
      }
    }
  }

}