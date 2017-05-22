package play.boilerplate.generators.security

import SecurityProvider._
import treehugger.forest._
import treehuggerDSL._

abstract class Play2AuthSecurityProvider(user: String,
                                         authConfig: String,
                                         securitySchema: String,
                                         imports: Seq[String] = Nil)
  extends SecurityProvider {

  override val controllerImports: Seq[Import] = {
    IMPORT("jp.t2v.lab.play2.auth", "AuthElement") +: imports.map(IMPORT(_))
  }

  override val controllerParents: Seq[Type] = {
    Seq(TYPE_REF(authConfig), TYPE_REF("AuthElement"))
  }

  override val controllerSelfTypes: Seq[Type] = Nil

  override val serviceImports: Seq[Import] = imports.map(IMPORT(_))

  case class SecurityScope(s: String) {
    val scope: String = s.split(':').head
    val values: Seq[String] = for (i <- s.split(':').tail; v <- i.split(',')) yield v
  }

  def parseAuthority(scopes: Seq[SecurityScope]): Seq[Tree]

  override def getActionSecurity(security: Seq[SecurityRequirement]): ActionSecurity = {

    security.find(_.name == securitySchema) match {
      case Some(SecurityRequirement(_, scopes)) =>

        val authority = parseAuthority(scopes.map(SecurityScope.apply)).map(
          authority => REF("AuthorityKey") INFIX "->" APPLY authority
        )

        val userParam: ValDef = PARAM("user", TYPE_REF(user)).tree
        val userValue: ValDef = VAL("user", TYPE_REF(user)) := (REF("loggedIn") APPLY REF("request"))

        new ActionSecurity {
          override def actionMethod(parser: Tree): Tree = {
            REF("AsyncStack") APPLY (parser +: authority)
          }
          override val securityParams: Map[String, ValDef] = {
            Map("user" -> userParam)
          }
          override val securityValues: Map[String, ValDef] = {
            Map("user" -> userValue)
          }
        }

      case None =>
        WithoutSecurity
    }

  }

}