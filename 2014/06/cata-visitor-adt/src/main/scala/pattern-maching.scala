package com.pellucid.patternmatching

sealed trait Expr

case class Num(n: Int) extends Expr

case class Sum(l: Expr, r: Expr) extends Expr

case class Prod(l: Expr, r: Expr) extends Expr

object Main extends App {

  def eval(expr: Expr): Int = expr match {
    case Num(n)     => n
    case Sum(l, r)  => eval(l) + eval(r)
    case Prod(l, r) => eval(l) * eval(r)
  }

  val expr =
    new Sum(new Prod(new Num(1), new Num(2)), new Num(3))
  println(eval(expr)) // prints 5
}
