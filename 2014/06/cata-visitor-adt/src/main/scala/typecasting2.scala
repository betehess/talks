package com.pellucid.typecasting2

trait Expr

class Num(val n: Int) extends Expr

class Sum(val l: Expr, val r: Expr) extends Expr

class Prod(val l: Expr, val r: Expr) extends Expr

object Expr {
  def eval(expr: Expr): Int = expr match {
    case num: Num   => num.n
    case sum: Sum   => eval(sum.l) + eval(sum.r)
    case prod: Prod => eval(prod.l) * eval(prod.r)
  }
}

object Main extends App {
  val expr =
    new Sum(new Prod(new Num(1), new Num(2)), new Num(3))
  println(Expr.eval(expr)) // prints 5
}
