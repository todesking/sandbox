package com.todesking.scalanb

object Runner {
  def main(args: Array[String]): Unit = {
    val Seq(className) = args.toSeq

    val targetClass = Class.forName(className)
    val run = targetClass.getMethod("scalanb__run", classOf[Builder])

    // TODO: tap stdout/stderr

    val target = targetClass.newInstance()

    val format = Format.Default
    val builder = new Builder.OnMemory(format)

    val _ = run.invoke(target, builder)

    println(ipynb.JsonMapping.toJson(builder.bang(), pretty = true))
  }
}
