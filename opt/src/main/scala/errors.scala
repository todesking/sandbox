package com.todesking.hoge

class UnveilException(msg: String) extends RuntimeException(msg)

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
