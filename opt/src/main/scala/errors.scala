package com.todesking.hoge

class UnsupportedOpcodeException(byte: Int, name: String)
  extends RuntimeException(f"Unsupported opcode: 0x$byte%02X at ${name}")

class AnalyzeException(msg: String) extends RuntimeException(msg)

class MethodAnalyzeException(classRef: ClassRef, methodRef: MethodRef, msg: String)
extends AnalyzeException(s"Method analyze failed(${classRef}.${methodRef}): ${msg}")

class FieldAnalyzeException(classRef: ClassRef, fieldRef: FieldRef, msg: String)
extends AnalyzeException(s"Field analyze failed(${classRef}.${fieldRef}): ${msg}")

class TransformError(msg: String) extends RuntimeException(msg)
