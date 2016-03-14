package com.todesking.hoge

class UnveilException(msg: String, err: Throwable) extends RuntimeException(msg, err) {
  def this(msg: String) = this(msg, null)
}

class MaterializeException(msg: String, err: Throwable) extends UnveilException(msg, err)

class InvalidClassException(val instance: Instance.Duplicate[_], err: LinkageError)
  extends MaterializeException(err.toString, err)

class AnalyzeException(msg: String) extends UnveilException(msg)

class MethodAnalyzeException(classRef: ClassRef, methodRef: MethodRef, msg: String)
  extends AnalyzeException(s"Method analyze failed(${classRef}.${methodRef}): ${msg}")

class UnsupportedOpcodeException(classRef: ClassRef, methodRef: MethodRef, byte: Int)
  extends MethodAnalyzeException(classRef, methodRef, f"Unsupported opcode: 0x$byte%02X")

class FieldAnalyzeException(classRef: ClassRef, fieldRef: FieldRef, msg: String)
  extends AnalyzeException(s"Field analyze failed(${classRef}.${fieldRef}): ${msg}")

class TransformException(msg: String) extends UnveilException(msg)

class FieldTransformException(classRef: ClassRef, fieldRef: FieldRef, msg: String)
  extends AnalyzeException(s"Transform failed at ${classRef}.${fieldRef}: ${msg}")

class MethodTransformException(classRef: ClassRef, methodRef: MethodRef, msg: String)
  extends AnalyzeException(s"Transform failed at ${classRef}.${methodRef}: ${msg}")

class BytecodeTransformException(val classRef: ClassRef, val methodRef: MethodRef, val methodBody: MethodBody, val bytecode: Bytecode, msg: String)
  extends MethodTransformException(classRef, methodRef, s"$bytecode: $msg")
