package com.todesking.hoge

class UnsupportedOpcodeException(byte: Int, name: String)
  extends RuntimeException(f"Unsupported opcode: 0x$byte%02X at ${name}")

class AnalyzeException(msg: String) extends RuntimeException(msg)

class TransformError(msg: String) extends RuntimeException(msg)
