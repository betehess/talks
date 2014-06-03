package com.pellucid.typecasting

class Num(val n: Int)

class Sum(val l: Any, val r: Any)

class Prod(val l: Any, val r: Any)

object Expr {
  def eval(expr: Any): Int = expr match {
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
