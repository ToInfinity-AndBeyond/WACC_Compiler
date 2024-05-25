package wacc.ast

import parsley.ap._
import parsley.generic
import parsley.Parsley
import parsley.position.pos

/*
  The ParserBridgePos object contains a set of traits designed to facilitate the integration
  of position information. This feature is particularly useful for error reporting and debugging 
  in compilers or interpreters. 
 */
object ParserBridgePos {

  /*
    A generic trait for creating parsers that capture position information for a single
    result type. This trait extends the generic ErrorBridge to incorporate error handling.
   */
  trait ParserSingletonBridgePos[+A] extends generic.ErrorBridge {
    protected def con(pos: (Int, Int)): A
    def from(op: Parsley[_]): Parsley[A]     = error(pos.map(this.con(_)) <* op)
    final def <#(op: Parsley[_]): Parsley[A] = this from op
  }

  
  // A bridge for creating parsers without input parameters that capture position information.
   
  trait ParserBridgePos0[A] extends ParserSingletonBridgePos[A] {
    def apply()(pos: (Int, Int)): A
    override def con(pos: (Int, Int)): A = this.apply()(pos)
  }

  /*
    A bridge for creating parsers with a single input parameter that capture position
    information. It allows for the construction of parsers that apply a function to the
    parsed input, incorporating the position into the result.
  */
  trait ParserBridgePos1[-A, +B] extends ParserSingletonBridgePos[A => B] {
    def apply(x: A)(pos: (Int, Int)): B
    def apply(x: Parsley[A]): Parsley[B] = error(ap1(pos.map(con), x))
    override final def con(pos: (Int, Int)): A => B = this.apply(_)(pos)
  }

  /*
    A bridge for creating parsers with two input parameters that capture position
    information. It facilitates the construction of parsers that combine two parsed
    elements, along with their position.
   */
  trait ParserBridgePos2[-A, -B, +C] extends ParserSingletonBridgePos[(A, B) => C] {
    def apply(x: A, y: B)(pos: (Int, Int)): C
    def apply(x: Parsley[A], y: => Parsley[B]): Parsley[C] = error(ap2(pos.map(con), x, y))
    override final def con(pos: (Int, Int)): (A, B) => C = this.apply(_, _)(pos)
  }

  /*
    A bridge for creating parsers with three input parameters that capture position
    information. This enables the construction of parsers for structures that are
    composed of three elements, each potentially coming from different parts of the input.
   */
  trait ParserBridgePos3[-A, -B, -C, +D] extends ParserSingletonBridgePos[(A, B, C) => D] {
    def apply(x: A, y: B, z: C)(pos: (Int, Int)): D
    def apply(x: Parsley[A], y: Parsley[B], z: Parsley[C]): Parsley[D] = error(
      ap3(pos.map(con), x, y, z)
    )
    override final def con(pos: (Int, Int)): (A, B, C) => D = this.apply(_, _, _)(pos)
  }

  /*
    A bridge for creating parsers with four input parameters that capture position
    information. It supports the construction of complex parsers that need to assemble
    a result from four different parsed inputs, incorporating their positional data.
   */
  trait ParserBridgePos4[-A, -B, -C, -D, +E] extends ParserSingletonBridgePos[(A, B, C, D) => E] {
    def apply(x: A, y: B, z: C, w: D)(pos: (Int, Int)): E
    def apply(x: Parsley[A], y: Parsley[B], z: Parsley[C], w: Parsley[D]): Parsley[E] =
      error(ap4(pos.map(con), x, y, z, w))
    override final def con(pos: (Int, Int)): (A, B, C, D) => E = this.apply(_, _, _, _)(pos)
  }
}