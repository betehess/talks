package com.pellucid.subtyping

trait Expr {
  def eval(): Int
}

class Num(val n: Int) extends Expr {
  def eval(): Int = n
}

class Sum(val l: Expr, val r: Expr) extends Expr {
  def eval(): Int = l.eval() + r.eval()
}

class Prod(val l: Expr, val r: Expr) extends Expr {
  def eval(): Int = l.eval() * r.eval()
}

object Main extends App {
  val expr =
    new Sum(new Prod(new Num(1), new Num(2)), new Num(3))
  println(expr.eval()) // prints 6
}
