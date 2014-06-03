package com.pellucid.visitor.generics

trait Expr {
  def accept[T](visitor: Visitor[T]): T
}

class Num(val n: Int) extends Expr {
  def accept[T](visitor: Visitor[T]): T = visitor.visit(this)
}

class Sum(val l: Expr, val r: Expr) extends Expr {
  def accept[T](visitor: Visitor[T]): T = visitor.visit(this)
}

class Prod(val l: Expr, val r: Expr) extends Expr {
  def accept[T](visitor: Visitor[T]): T = visitor.visit(this)
}

trait Visitor[T] {
  def visit(num: Num): T
  def visit(sum: Sum): T
  def visit(prod: Prod): T
}


final class Eval() extends Visitor[Int] {
  def visit(num: Num): Int = num.n
  def visit(sum: Sum): Int = sum.l.accept(this) + sum.r.accept(this)
  def visit(prod: Prod): Int = prod.l.accept(this) * prod.r.accept(this)
}

object Main extends App {
  val expr =
    new Sum(new Prod(new Num(1), new Num(2)), new Num(3))
  println(expr.accept(new Eval())) // prints 5
}
