package play.boilerplate.api.client.dsl

import play.api.libs.json.JsValue
import play.api.libs.ws._

object Compat extends AbstractCompat with JsonBodyWritables with JsonBodyReadables {
  override type WSClient = StandaloneWSClient
  override type WSRequest = StandaloneWSRequest
  override type WSResponse = StandaloneWSResponse

  implicit class WSResponseOps(val response: WSResponse) extends AnyVal {
    def json: JsValue = response.body[JsValue]
  }
}
