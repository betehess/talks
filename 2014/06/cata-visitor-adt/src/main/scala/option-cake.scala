package com.pellucid.option.cake

trait OptionModule {

  type OptionInt

  type SomeInt <: OptionInt

  type None <: OptionInt

  def Option: OptionLike

  trait OptionLike {
    
    def some(x: Int): OptionInt
    
    def none: OptionInt
    
    def fold[T](opt: OptionInt)(ifNone: => T, ifSome: Int => T): T

  }

}

trait ShowOption extends OptionModule {

  object Show {
    def show(opt: OptionInt): String = Option.fold(opt)("None", x => s"Some($x)")
  }

}

trait ScalaOptionModule extends OptionModule {

  type OptionInt = scala.Option[Int]

  type SomeInt = scala.Some[Int]

  type None = scala.None.type

  object Option extends OptionLike {

    def some(x: Int): OptionInt = Some(x)

    val none: OptionInt = scala.None

    def fold[T](opt: OptionInt)(ifNone: => T, ifSome: Int => T): T = opt match {
      case scala.None    => ifNone
      case scala.Some(x) => ifSome(x)
    }

  }

}


trait Program extends App with OptionModule with ShowOption {
  val opt = Option.some(42)
  println(Show.show(opt)) // will print Some(42)
}


object MainWithScalaOption extends Program with ScalaOptionModule
