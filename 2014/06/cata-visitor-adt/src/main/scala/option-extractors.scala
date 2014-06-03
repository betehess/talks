package com.pellucid.option.extractors

import scala.language.higherKinds
import scala.language.implicitConversions

trait OptionSig {

  type Option[+_]

  type Some[+A] <: Option[A]

  type None <: Option[Nothing]

}


trait OptionOps[Sig <: OptionSig] { self =>

  def some[A](x: A): Sig#Some[A]
    
  def none: Sig#None
    
  def fold[A, B](opt: Sig#Option[A])(ifNone: => B, ifSome: A => B): B

  object Some {
    def apply[A](x: A): Sig#Some[A] = self.some(x)
    def unapply[A](opt: Sig#Option[A]): scala.Option[A] =
      self.fold(opt)(scala.None, x => scala.Some(x))
  }

  val None = none

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

  implicit object Syntax extends Syntax[ScalaOption]

}



class Show[Sig <: OptionSig](implicit ops: OptionOps[Sig]) {

  import ops._

  // ok, could be better and not rely on .toString...
  def show[A](opt: Sig#Option[A]): String = opt match {
    case None    => "None"
    case Some(x) => s"Some(${x.toString})"
  }

}

object Show {

  implicit def apply[Sig <: OptionSig](implicit ops: OptionOps[Sig]): Show[Sig] =
    new Show[Sig]

}




class Flatten[Sig <: OptionSig](implicit ops: OptionOps[Sig]) {

  import ops._

  def flatten[A](oo: Sig#Option[Sig#Option[A]]): Sig#Option[A] = oo match {
    case None          => None
    case Some(None)    => None
    case Some(Some(x)) => Some(x)
  }

}

object Flatten {

  implicit def apply[Sig <: OptionSig](implicit ops: OptionOps[Sig]): Flatten[Sig] =
    new Flatten[Sig]

}



class OptionW[Sig <: OptionSig, T](val opt: Sig#Option[T]) extends AnyVal {

  def show(implicit S: Show[Sig]): String = S.show(opt)

}

class OptionOptionW[Sig <: OptionSig, T](val oo: Sig#Option[Sig#Option[T]]) extends AnyVal {

  def flatten(implicit F: Flatten[Sig]): Sig#Option[T] = F.flatten(oo)

}

class Syntax[Sig <: OptionSig] {

  implicit def optionW[T](opt: Sig#Option[T]): OptionW[Sig, T] = new OptionW[Sig, T](opt)

  implicit def optionOptionW[T](oo: Sig#Option[Sig#Option[T]]): OptionOptionW[Sig, T] = new OptionOptionW[Sig, T](oo)

}






class Program[Sig <: OptionSig](implicit Ops: OptionOps[Sig], Syntax: Syntax[Sig], Flatten: Flatten[Sig]) extends App {
  import Ops._
  import Syntax._
  println(Some(Some(42)).flatten.show) // will print Some(42)
}


object MainWithScalaOption extends Program[ScalaOption]
