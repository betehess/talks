package com.pellucid.option.flatten

import scala.language.higherKinds

trait OptionSig {

  type Option[+_]

  type Some[+A] <: Option[A]

  type None <: Option[Nothing]

}


trait OptionOps[Sig <: OptionSig] {

  def some[A](x: A): Sig#Some[A]
    
  def none: Sig#None
    
  def fold[A, B](opt: Sig#Option[A])(ifNone: => B, ifSome: A => B): B

}




trait ScalaOption extends OptionSig {

  type Option[+A] = scala.Option[A]

  type Some[+A] = scala.Some[A]

  type None = scala.None.type

}

object ScalaOption {

  implicit object Ops extends OptionOps[ScalaOption] {

    def some[A](x: A): ScalaOption#Some[A] = scala.Some(x)

    val none: ScalaOption#None = scala.None

    def fold[A, B](opt: ScalaOption#Option[A])(ifNone: => B, ifSome: A => B): B = opt match {
      case scala.None    => ifNone
      case scala.Some(x) => ifSome(x)
    }

  }

}



class Show[Sig <: OptionSig](implicit ops: OptionOps[Sig]) {

  import ops._

  // ok, could be better and not rely on .toString...
  def show[A](opt: Sig#Option[A]): String =
    fold(opt)("None", x => s"Some(${x.toString})")

}

object Show {

  implicit def apply[Sig <: OptionSig](implicit ops: OptionOps[Sig]): Show[Sig] =
    new Show[Sig]

}




class Flatten[Sig <: OptionSig](implicit ops: OptionOps[Sig]) {

  import ops._

  def flatten[A](oo: Sig#Option[Sig#Option[A]]): Sig#Option[A] = fold(oo)(
    none,
    someOpt => fold(someOpt)(
      none,
      someA => some(someA)
    )
  )

}

object Flatten {

  implicit def apply[Sig <: OptionSig](implicit ops: OptionOps[Sig]): Flatten[Sig] =
    new Flatten[Sig]

}










class Program[Sig <: OptionSig](implicit Ops: OptionOps[Sig], Show: Show[Sig], Flatten: Flatten[Sig]) extends App {
  import Ops._
  import Show._
  println(show(Flatten.flatten(some(some(42))))) // will print Some(42)
}


object MainWithScalaOption extends Program[ScalaOption]
