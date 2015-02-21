package com.todesking.sciatic

import scala.reflect.ClassTag

trait Template[A] {
  def render(context: A)
}

sealed abstract class AST
object AST {
}

class JadeParser {
  def parse(content: String): AST = ???
}

case class Code[A]()

class CodeGenerator {
  def generate[A: ClassTag](packageName: String, className: String, ast: AST): Code[A] = ???
}

class Compiler {
  def compile[Ctx: ClassTag](code: Code[Ctx]): Template[Ctx] = ???
}

trait TemplateRepository {
  def load[A: ClassTag](path: String): Template[A]
}

class CompiledTemplate[A](path: String) extends Template[A] {
  def cast[B: ClassTag](): CompiledTemplate[B] = ???
  override def render(context: A) = ???
}

class LiveTemplateRepository extends TemplateRepository {
  private[this] var compiledTemplates: Map[String, (Long, CompiledTemplate[_])] = Map.empty

  override def load[A: ClassTag](path: String): LiveTemplate[A] =
    new LiveTemplate[A](path, this)

  def reload[A: ClassTag](path: String): Template[A] =
    (for {
      (timestamp, template) <- compiledTemplates.get(path) if isFresh(path, timestamp)
    } yield {
      template.cast[A]
    }) getOrElse {
      forceReload[A](path)
    }

  private[this] def isFresh(path: String, timestamp: Long): Boolean = ???

  def forceReload[A: ClassTag](path: String): CompiledTemplate[A] = ???
}

class LiveTemplate[A: ClassTag](path: String, repo: LiveTemplateRepository) extends Template[A] {
  override def render(context: A) =
    repo.reload[A](path).render(context)
}
