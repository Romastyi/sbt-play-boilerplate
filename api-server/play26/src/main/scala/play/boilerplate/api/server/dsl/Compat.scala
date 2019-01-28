package play.boilerplate.api.server.dsl

import play.api.mvc.AnyContent

object Compat extends AbstractCompat {
  override type Routes = play.api.routing.Router

  implicit class AnyContentOps(val body: AnyContent) extends AnyVal {

    def dataPart(key: String): Option[String] = {
      body.asMultipartFormData.flatMap(_.dataParts.get(key)).flatMap(_.headOption)
    }

    def file(key: String): Option[java.io.File] = {
      body.asMultipartFormData.flatMap(_.file(key)).map(_.ref.path.toFile)
    }

    def formValue(key: String): Option[String] = {
      body.asFormUrlEncoded.flatMap(_.get(key)).flatMap(_.headOption)
    }

  }
}
