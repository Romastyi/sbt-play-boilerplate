package scaldi.play

import play.api.Application
import play.api.ApplicationLoader.Context

class FixedScaldiApplicationLoader(override val builder: ScaldiApplicationBuilder)
  extends ScaldiApplicationLoader(builder) {

  def this() = this(new ScaldiApplicationBuilder())

  override def load(context: Context): Application =
    builder
      .in(context.environment)
      .loadConfig(context.initialConfiguration)
      .build
}
