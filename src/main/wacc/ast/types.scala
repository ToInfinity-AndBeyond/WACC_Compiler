package wacc.ast

import parsley.generic._

object Types {
  /* Types */
  sealed trait Type {
    def prettyPrint: String
  }

  // Base Type
  sealed abstract class BaseType extends Type with PairElemType
  case object IntType    extends BaseType { override def prettyPrint: String = "int"}
  case object BoolType   extends BaseType { override def prettyPrint: String = "bool"}
  case object CharType   extends BaseType { override def prettyPrint: String = "char"}
  case object StringType extends BaseType { override def prettyPrint: String = "string"}

  // Array Type
  case class ArrayType(t: Type) extends Type with PairElemType {
    override def prettyPrint: String = s"${t.prettyPrint}[]"
  }
  object ArrayType              extends ParserBridge1[Type, ArrayType]

  // Pair Type
  case class PairType(t1: PairElemType, t2: PairElemType) extends Type {
    override def prettyPrint: String = s"pair(${t1.prettyPrint}, ${t2.prettyPrint})"
  }
  object PairType extends ParserBridge2[PairElemType, PairElemType, PairType]

  // Pair Elem Type
  sealed trait PairElemType extends Type
  case object ErasedPairType extends Type with PairElemType { override def prettyPrint: String = "pair" }

  // Any Type
  case object AnyType extends Type with PairElemType { override def prettyPrint: String = "any" } 
  // Func Type
  case class FuncType(t: Type, args: List[Type]) extends Type {
     override def prettyPrint: String = s"$t(${args.mkString(", ")})"
  }
}
