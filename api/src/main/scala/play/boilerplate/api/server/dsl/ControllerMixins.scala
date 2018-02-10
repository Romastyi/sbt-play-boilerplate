package play.boilerplate.api.server.dsl

import play.api.mvc.RequestHeader

trait ControllerMixins {

  protected def acceptsMimeType(mimeType: String)(implicit r: RequestHeader): Boolean = {
    r.acceptedTypes.nonEmpty && r.accepts(mimeType)
  }

}
