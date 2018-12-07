package play.boilerplate.generators.security

import SecurityProvider._
import play.boilerplate.generators.injection.InjectionProvider.Dependency
import treehugger.forest._
import treehuggerDSL._

abstract class Play2AuthSecurityProvider(userModel: String,
                                         authConfig: String,
                                         securitySchema: String,
                                         imports: Seq[String] = Nil)
  extends DefaultSecurity(securitySchema) {

  override def controllerImports: Seq[Import] = {
    IMPORT(REF("jp.t2v.lab.play2.auth"), "AuthElement") +: serviceImports
  }

  override def controllerParents: Seq[Type] = {
    Seq(TYPE_REF(authConfig), TYPE_REF("AuthElement"))
  }

  override def controllerSelfTypes: Seq[Type] = Nil

  override def controllerDependencies: Seq[Dependency] = Nil

  override def serviceImports: Seq[Import] = imports.map(pkg => IMPORT(REF(pkg)))

  def parseAuthority(scopes: Seq[SecurityScope]): Seq[Tree]

  override def composeActionSecurity(scopes: Seq[SecurityScope]): ActionSecurity = {

    val authority = parseAuthority(scopes).map(
      authority => REF("AuthorityKey") INFIX "->" APPLY authority
    )

    val userType: Type = TYPE_REF(userModel)
    val userValue: ValDef = VAL("logged", userType) := (REF("loggedIn") APPLY REF("request"))

    new ActionSecurity {
      override def actionMethod(parser: Tree): Tree = {
        REF("AsyncStack") APPLY (parser +: authority)
      }
      override val securityParams: Seq[(String, Type)] = {
        Seq("logged" -> userType)
      }
      override val securityValues: Seq[(String, ValDef)] = {
        Seq("logged" -> userValue)
      }
      override val securityDocs: Seq[(String, String)] = {
        Seq("logged" -> "Current logged user")
      }
    }

  }

}