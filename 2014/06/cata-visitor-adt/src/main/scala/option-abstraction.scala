package com.pellucid.option.abstraction

/** deconstructing Scala case classes into
  * - existence of some types
  * - subtyping information
  * - injectors into the types
  * - fold
  */
trait OptionModule {

  type OptionInt

  type SomeInt <: OptionInt

  type None <: OptionInt

  def someInt(x: Int): OptionInt

  def none: OptionInt

  def fold[T](opt: OptionInt)(ifNone: => T, ifSome: Int => T): T

}

/** given an OptionModule, I can write a Show[OptionInt] */
trait ShowOption extends OptionModule {

  def show(opt: OptionInt): String = fold(opt)("None", x => s"Some($x)")

}

/** an implementation of OptionModule relying on a custom class
  * hierarchy + a visitor
  */
trait OptionVisitorModule extends OptionModule {

  trait OptionInt {
    def accept[T](visitor: Visitor[T]): T
  }

  class SomeInt(val x: Int) extends OptionInt {
    def accept[T](visitor: Visitor[T]): T = visitor.visit(this)
  }

  class None() extends OptionInt {
    def accept[T](visitor: Visitor[T]): T = visitor.visit(this)
  }

  trait Visitor[T] {
    def visit(none: None): T
    def visit(someInt: SomeInt): T
  }

  def someInt(x: Int): OptionInt = new SomeInt(x)

  val none: OptionInt = new None()

  def fold[T](opt: OptionInt)(ifNone: => T, ifSome: Int => T): T =
    opt.accept(new Visitor[T] {
      def visit(none: None): T = ifNone
      def visit(someInt: SomeInt): T = ifSome(someInt.x)
    })

}

/** an implementation of OptionModule relying on scala.Option
  */
trait ScalaOptionModule extends OptionModule {

  type OptionInt = scala.Option[Int]

  type SomeInt = scala.Some[Int]

  type None = scala.None.type

  def someInt(x: Int): OptionInt = Some(x)

  val none: OptionInt = scala.None

  def fold[T](opt: OptionInt)(ifNone: => T, ifSome: Int => T): T = opt match {
    case scala.None    => ifNone
    case scala.Some(x) => ifSome(x)
  }

}

/** an abstract Program, waiting for a OptionModule to work */
trait Program extends App with OptionModule with ShowOption {
  val opt = someInt(42)
  println(show(opt)) // will print Some(42)
}


object MainWithVisitor extends Program with OptionVisitorModule

object MainWithScalaOption extends Program with ScalaOptionModule
