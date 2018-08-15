package scala.tools.nsc.interpreter

// From scala.tools.nsc.interpreter.IMain
/*
Copyright (c) 2002-2018 EPFL
Copyright (c) 2011-2018 Lightbend, Inc.

All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

  * Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  * Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
  * Neither the name of the EPFL nor the names of its contributors
    may be used to endorse or promote products derived from this software
    without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

class ScalanbREPL extends ILoop(None, new JPrintWriter(Console.out, true)) {
  protected def hook(interp: Interp)(
    src: String,
    trees: Seq[interp.global.Tree],
    inSilent: Boolean)(eval: () => Either[Throwable, (Any, String)]): Either[Throwable, (Any, String)] =
    eval()

  @deprecated("", "")
  private[this] def updateCP(): Unit = {
    if (addedClasspath != "")
      settings.classpath append addedClasspath
  }
  override def createInterpreter() = {
    updateCP() // I want to suppress deprecated warning...
    intp = new Interp
  }

  class Interp extends ILoopInterpreter {
    import reporter.{ printMessage, printUntruncatedMessage }

    private[this] var _inSilent: Boolean = false
    private[this] def silent[A](f: => A): A = {
      _inSilent = true
      try { f } finally { _inSilent = false }
    }

    override def interpret(line: String, synthetic: Boolean): IR.Result = {
      def loadAndRunReq(req: Request) = classLoader.asContext {
        /**
         * To our displeasure, ConsoleReporter offers only printMessage,
         *  which tacks a newline on the end.  Since that breaks all the
         *  output checking, we have to take one off to balance.
         */
        hook(this)(line, req.trees, _inSilent) { () =>
          for {
            value <- req.lineRep.evalEither.right
            str <- req.lineRep.callEither(naming.sessionNames.print).right
          } yield (value, str.toString)
        } match {
          case Right((retval, str)) =>
            val result = retval.toString
            if (printResults && result != "")
              printMessage(result stripSuffix "\n")
            else if (isReplDebug) // show quiet-mode activity
              printMessage(result.trim.lines map ("[quiet] " + _) mkString "\n")

            // Book-keeping.  Have to record synthetic requests too,
            // as they may have been issued for information, e.g. :type
            recordRequest(req)
            IR.Success
          case Left(t) =>
            // don't truncate stack traces
            printUntruncatedMessage(req.lineRep.bindError(t))
            IR.Error

        }
      }

      compile(line, synthetic) match {
        case Left(result) => result
        case Right(req) => loadAndRunReq(req)
      }
    }

    private[this] def compile(line: String, synthetic: Boolean): Either[IR.Result, Request] = {
      if (global == null) Left(IR.Error)
      else requestFromLine(line, synthetic) match {
        case Right(null) => Left(IR.Error) // disallowed statement type
        case Right(req) if !req.compile => Left(IR.Error) // compile error
        case ok @ Right(req) => ok
        case err @ Left(result) => err
      }
    }
    override def bind(name: String, boundType: String, value: Any, modifiers: List[String] = Nil): IR.Result =
      silent { super.bind(name, boundType, value, modifiers) }
  }
}
