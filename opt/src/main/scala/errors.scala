package com.todesking.hoge

class UnsupportedOpcodeException(byte: Int)
  extends RuntimeException(f"Unsupported opcode: 0x$byte%02X")

class AnalyzeException(msg: String) extends RuntimeException(msg)

class TransformError(msg: String) extends RuntimeException(msg)
