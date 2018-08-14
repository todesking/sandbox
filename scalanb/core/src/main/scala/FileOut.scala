package com.todesking.scalanb

import java.nio.file.Path
import java.nio.file.Files

class FileOut(val path: Path, val name: String) {
  def prepare(): Unit = {
    val _ = path.toFile.mkdirs()
  }

  def notebook(ast: ipynb.Notebook): Unit = {
    import scala.collection.JavaConverters._
    val src = ipynb.JsonMapping.toJson(ast, pretty = true)
    prepare()
    val _ = Files.write(path.resolve(s"$name.ipynb"), Seq(src).asJava)
  }
}
