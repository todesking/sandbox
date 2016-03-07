package com.todesking.hoge

import java.lang.reflect.{ Constructor, Method => JMethod }

object Analyze {
  def setterAssigns(self: Instance[_ <: AnyRef], ctor: Constructor[_]): Option[Map[(ClassRef, FieldRef), Either[Int, Any]]] = {
    Javassist.decompile(ctor).map { b =>
      val df = b.dataflow(self)
      def isThrowNull(l: Bytecode.Label): Boolean = {
        import Bytecode._
        b.labelToBytecode(l) match {
          case bc @ aconst_null() =>
            isThrowNull(df.fallThroughs(bc.label))
          case athrow() =>
            true
          case other =>
            false
        }
      }
      import Bytecode._
      // TODO: MethodBody.basicBlocks
      b.bytecode.foldLeft(Map.empty[(ClassRef, FieldRef), Either[Int, Any]]) {
        case (assigns, bc) =>
          bc match {
            case bc: Shuffle => assigns
            case bc: Jump => assigns
            case bc: Return => assigns
            case bc: ConstX => assigns
            case bc @ invokespecial(classRef, methodRef) if df.dataValue(bc.objectref).isInstance(self) && methodRef.isInit =>
              val klass = classRef match { case cr @ ClassRef.Concrete(_, _) => cr.loadClass; case unk => throw new AssertionError(s"${unk}") }
              val ctor = klass.getDeclaredConstructors.find { c => MethodRef.from(c) == methodRef }.get
              // TODO: FIXME: WRONG.
              setterAssigns(self, ctor).map { as => assigns ++ as } getOrElse { return None }
            case bc @ putfield(classRef, fieldRef) if df.dataValue(bc.objectref).isInstance(self) =>
              assigns + (
                df.dataValue(bc.value).value.map { v =>
                  (classRef -> fieldRef) -> Right(v) // constant
                } getOrElse {
                  def argNum(label: DataLabel.Out): Option[Int] = {
                    val index = df.argLabels.indexOf(label)
                    if (index == -1) None else Some(index)
                  }
                  val l = df.dataBinding(bc.value)
                  argNum(l).map { i => (classRef -> fieldRef) -> Left(i) } getOrElse { return None }
                }
              )
            case athrow() => return None
            case bc @ ifnonnull(target) if isThrowNull(df.fallThroughs(bc.label)) =>
              // TODO: NIMPL
              // TODO: exception handler
              // TODO: mark the value as non-nullable
              // assigns
              return None
            case bc: Branch => return None
            case bc: Procedure => return None
          }
      }
    }
  }

  def findSetterConstructor[A](
    self: Instance[_ <: AnyRef],
    klass: Class[A], fields: Map[(ClassRef, FieldRef), Field]
  ): Option[(MethodDescriptor, Map[(ClassRef, FieldRef), Either[Int, Any]])] = {
    val ctors: Map[MethodDescriptor, Constructor[_]] =
      klass
        .getDeclaredConstructors
        .filterNot { c => MethodAttribute.Private.enabledIn(c.getModifiers) }
        .map { c => MethodDescriptor.from(c) -> c }
        .toMap

    val ctorAssigns: Map[MethodDescriptor, Map[(ClassRef, FieldRef), Either[Int, Any]]] =
      ctors
        .mapValues(Analyze.setterAssigns(self, _))
        .collect { case (k, Some(v)) => (k -> v) }

    ctorAssigns.find {
      case (ctor, assigns) =>
        val (common, unkFieldValues, unkAssigns) = Algorithm.mapZip(fields.mapValues(_.data.concreteValue), assigns)
        if (unkFieldValues.nonEmpty) {
          throw new RuntimeException(s"Unknown field values: ${unkFieldValues}")
        }
        if (unkAssigns.nonEmpty) {
          throw new RuntimeException(s"Unknown assigns in constructor: ${unkAssigns}")
        }
        common.forall {
          case ((classRef, fieldRef), (v1, Right(v2))) =>
            fieldRef.descriptor.typeRef match {
              case p: TypeRef.Primitive => v1 == v2
              case r: TypeRef.Reference => v1.asInstanceOf[AnyRef] eq v2.asInstanceOf[AnyRef] // TODO: care String
            }
          case ((classRef, fieldRef), (v1, Left(n))) =>
            true
        }
    }
  }
}

