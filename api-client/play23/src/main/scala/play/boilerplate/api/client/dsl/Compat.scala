package play.boilerplate.api.client.dsl

object Compat extends AbstractCompat {
  override type WSClient = play.api.libs.ws.WSClient
  override type WSRequest = play.api.libs.ws.WSRequestHolder
  override type WSResponse = play.api.libs.ws.WSResponse

  implicit class WSRequestOps(val request: WSRequest) extends AnyVal {
    def withHttpHeaders(headers: (String, String)*): WSRequest = request.withHeaders(headers: _ *)
  }

}
