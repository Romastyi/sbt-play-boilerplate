/* Copyright 2015 UniCredit S.p.A.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package eu.unicredit.swagger

object StringUtils {

  private def sanitizePath(s: String): String =
    s.replaceAll("\\{([^}]+)\\}", ":$1").trim

  def cleanDuplicateSlash(s: String): String =
    s.replaceAll("//+", "/")

  private def cleanUrl(s: String): String =
    s.replace("/?", "?").replaceAll("/$", "")

  def cleanPathParams(s: String): String =
    s.replace(":", "$").trim

  def padTo(n: Int, s: String): String =
    s + " " * (n - s.length max 0)

  def doUrl(basePath: String, path: String): String =
    cleanUrl(cleanDuplicateSlash(basePath + sanitizePath(path)))

  def toCamelCase(s: String): String = {
    s.headOption.map(_.toUpper).getOrElse("") + s.tail
  }

}
