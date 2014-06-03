package com.pellucid.option.patternmatching

sealed trait Option[+A]

case class Some[+A](x: A) extends Option[A]

case object None extends Option[Nothing]

object Main extends App {

  def flatten[T](oo: Option[Option[T]]): Option[T] = oo match {
    case Some(Some(x)) => Some(x)
    case Some(None)    => None
    case None          => None
  }

  assert(flatten(Some(Some(42))) == Some(42))
  assert(flatten(Some(None)) == None)

}
