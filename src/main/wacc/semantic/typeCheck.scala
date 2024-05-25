package wacc.semantic

import wacc.ast.Types._
import wacc.errors.WaccError._
import scala.annotation.tailrec

object TypeCheck {
  /**
   * Attempts to match a given type (`t2`) against a list of types (`ts`) using a specified type-checking function (`isType`).
   * 
   * @param isType A function that takes two types and a position, and returns whether the first type matches the second.
   * @param ts A list of types to check against `t2`.
   * @param t2 The type to match against the list of types `ts`.
   * @param pos The position in the source code where the type check occurs, used for error reporting.
   * @return Either a list of `WaccError` if no match is found (`Left`), or the matched type (`Right`).
   */
  def checkMultiple(
    isType: Type => (Type, (Int, Int)) => Either[List[WaccError], Type],
    ts: List[Type]
  ) (t2: Type, pos: (Int, Int)): Either[List[WaccError], Type]  = {
    ts.find(t => isType(t)(t2, pos).isRight) match {
      case Some(t) => Right(t)
      case None    => Left(List(TypeError(None, Some(t2.prettyPrint), 
                           Some(ts.map(_.prettyPrint).mkString(" or ")), None, pos)))
    }
  }

  /**
   * Checks if two types are exactly the same.
   * 
   * @param t1 The first type to compare.
   * @param t2 The second type to compare, along with its position in the source code.
   * @return Either a list of `WaccError` if the types do not match (`Left`), or the matched type (`Right`).
   */
  def isExactly(t1: Type)(t2: Type, pos: (Int, Int)): Either[List[WaccError], Type]  = {
    (t1, t2) match {
      case (t1, t2) if t1 == t2 => Right(t1)
      case _ => Left(List(TypeError(None,  Some(t2.prettyPrint), Some(t1.prettyPrint), None, pos)))
    }
  }

  /**
   * Helper function for determining if two types can be considered equivalent through weakening. 
   * 
   * @param t1 The first type to compare.
   * @param t2 The second type to compare.
   * @param pos The position in the source code for error reporting.
   * @return Either a list of `WaccError` if the types cannot be reconciled (`Left`), or the reconciled type (`Right`).
   */
  private def weakenHelper(t1: Type, t2: Type, pos: (Int, Int)): Either[List[WaccError], Type] = {
    (t1, t2) match {
      case (AnyType, AnyType) =>
        Left(List(TypeError(None, None, None, Some("Assignment is not legal when both sides types are unknown"), pos)))
      case (t1, t2) if t1 == t2             => Right(t1)
      case (_, AnyType)                     => Right(t1)
      case (AnyType, _)                     => Right(t2)
      case (ErasedPairType, PairType(_, _)) => Right(ErasedPairType)
      case (PairType(_, _), ErasedPairType) => Right(ErasedPairType)
      case (PairType(AnyType, AnyType), p2@PairType(_, _)) => Right(p2)
      case (ArrayType(t1), ArrayType(t2)) => weakenHelper(t1, t2, pos).flatMap(t => Right(ArrayType(t)))
      case (_, _) => Left(List(TypeError(None, Some(t2.prettyPrint), Some(t1.prettyPrint), None, pos)))
    }
  }

  /**
   * Checks if a type (`t1`) can be "weakened" to another type (`t2`), allowing for some flexibility in type compatibility.
   * 
   * @param t1 The source type for comparison.
   * @param t2 The target type for comparison, along with its position in the source code.
   * @return Either a list of `WaccError` if `t1` cannot be weakened to `t2` (`Left`), or the weakened type (`Right`).
   */
  def canWeakenTo(t1: Type)(t2: Type, pos: (Int, Int)): Either[List[WaccError], Type] = {
    (t1, t2) match {
      case (StringType, ArrayType(CharType)) => Right(StringType)
      case (_, _)                            => weakenHelper(t1, t2, pos)
    }
  }

  /**
   * Determines if two types share a common ancestor in the type hierarchy,
   * implying they can be used interchangeably in equality checking and array type infering.
   * 
   * @param t1 The first type to compare.
   * @param t2 The second type to compare, along with its position.
   * @return Either a list of `WaccError` if the types do not share an ancestor (`Left`), or the common ancestor type (`Right`).
   */
  def shareAncestor(t1: Type)(t2: Type, pos: (Int, Int)): Either[List[WaccError], Type] = {
    (t1, t2) match {
      case (ArrayType(CharType), StringType) => Right(StringType)
      case (_, _)                            => canWeakenTo(t1)(t2, pos)
    }
  }

  /**
   * Infers the type of a given expression or variable, mainly used when the type is obvious from the context.
   * 
   * @param t The type to infer.
   * @param pos The position in the source code for potential error reporting.
   * @return The inferred type wrapped in a `Right`, as inference is assumed to be successful.
   */
  def inferType(t: Type, pos: (Int, Int)): Either[List[WaccError], Type] = Right(t)
}
