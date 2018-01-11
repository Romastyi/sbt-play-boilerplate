package play.boilerplate.generators.security

import play.boilerplate.generators.injection.InjectionProvider
import play.boilerplate.generators.security.SecurityProvider.{ActionSecurity, DefaultSecurity, SecurityScope}
import treehugger.forest._
import treehuggerDSL._

abstract class SilhouetteSecurityProvider(securitySchema: String) extends DefaultSecurity(securitySchema) {

  def envType: Type

  def genericAuthenticator: Type = envType TYPE_# "A"

  def userType: Type

  override def controllerImports: Seq[Import] = Seq(
    IMPORT(REF("com.mohiva.play.silhouette.api"), "Silhouette")
  )
  override def controllerParents: Seq[Type] = Nil
  override def controllerSelfTypes: Seq[Type] = Nil
  override def controllerDependencies: Seq[InjectionProvider.Dependency] = Seq(
    InjectionProvider.Dependency("silhouette", TYPE_REF("Silhouette") TYPE_OF envType)
  )

  def parseAuthority(scopes: Seq[SecurityScope]): Seq[Tree]

  override def composeActionSecurity(scopes: Seq[SecurityScope]): ActionSecurity = {

    val authority = parseAuthority(scopes)
    val userValue: ValDef = VAL("user", userType) := REF("request") DOT "identity"

    new ActionSecurity {
      override def actionMethod(parser: Tree): Tree = {
        REF("silhouette") DOT "SecuredAction" APPLY authority DOT "async" APPLY parser
      }
      override val securityParams: Map[String, Type] = {
        Map("user" -> userType)
      }
      override val securityValues: Map[String, ValDef] = {
        Map("user" -> userValue)
      }
    }

  }

}
