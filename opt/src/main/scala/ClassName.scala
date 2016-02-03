
package com.todesking.hoge
case class ClassName(str: String) {
  def binaryString: String = str.replaceAll("\\.", "/")
}
