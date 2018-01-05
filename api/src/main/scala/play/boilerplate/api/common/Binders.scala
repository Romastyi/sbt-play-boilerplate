package play.boilerplate.api.common

import play.api.mvc.QueryStringBindable

import language.experimental.macros

object Binders {

  /**
    * Generates QueryStringBindable, for which this parameter must be present in the query-parameters
    */
  def queryStringStrict[A]: QueryStringBindable[A] = macro BindersMacroImpl.queryStringStrictImpl[A]

  /**
    * Generates QueryStringBindable, for which this parameter is not necessarily present in the query-parameters
    */
  def queryString[A]: QueryStringBindable[A] = macro BindersMacroImpl.queryStringImpl[A]

}