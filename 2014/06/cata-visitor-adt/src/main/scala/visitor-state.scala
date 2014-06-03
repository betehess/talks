package com.pellucid.visitor.state

trait Expr {
  def accept(visitor: Visitor): Unit
}

class Num(val n: Int) extends Expr {
  def accept(visitor: Visitor): Unit = visitor.visit(this)
}

class Sum(val l: Expr, val r: Expr) extends Expr {
  def accept(visitor: Visitor): Unit = visitor.visit(this)
}

class Prod(val l: Expr, val r: Expr) extends Expr {
  def accept(visitor: Visitor): Unit = visitor.visit(this)
}

trait Visitor {
  def visit(num: Num): Unit
  def visit(sum: Sum): Unit
  def visit(prod: Prod): Unit
}


final class Eval() extends Visitor {
  var result: Int = 0
  def visit(num: Num): Unit = {
    result = num.n
  }
  def visit(sum: Sum): Unit = {
    sum.l.accept(this)
    val lresult = result
    sum.r.accept(this)
    val rresult = result
    result = lresult + rresult
  }
  def visit(prod: Prod): Unit = {
    prod.l.accept(this)
    val lresult = result
    prod.r.accept(this)
    val rresult = result
    result = lresult * rresult
  }
}

object Main extends App {
  val expr =
    new Sum(new Prod(new Num(1), new Num(2)), new Num(3))
  val statefulVisitor = new Eval()
  expr.accept(statefulVisitor)
  println(statefulVisitor.result) // prints 5
}
