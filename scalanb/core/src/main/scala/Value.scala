package com.todesking.scalanb

import play.api.libs.json.JsValue
import play.api.libs.json.JsString

// Value representation of various format
case class Value(data: Map[String, JsValue]) {
  def ++(rhs: Value): Value =
    Value(data ++ rhs.data)
}

object Value {
  def apply(contentType: String, value: JsValue): Value =
    Value(Map(contentType -> value))

  def text(s: String): Value =
    apply("text/plain", JsString(s))

  def html(s: String): Value =
    apply("text/html", JsString(s))
}

