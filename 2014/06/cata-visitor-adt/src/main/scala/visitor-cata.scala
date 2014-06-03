package com.pellucid.visitor.cata

trait Expr {
  def accept[T](
    fnum: Num => T,
    fsum: Sum => T,
    fprod: Prod => T): T =
    this match {
      case num: Num   => fnum(num)
      case sum: Sum   => fsum(sum)
      case prod: Prod => fprod(prod)
    }
}

class Num(val n: Int) extends Expr

class Sum(val l: Expr, val r: Expr) extends Expr

class Prod(val l: Expr, val r: Expr) extends Expr

object Main extends App {

  def eval(expr: Expr): Int = expr.accept(
    num  => num.n,
    sum  => eval(sum.l) + eval(sum.r),
    prod => eval(prod.l) * eval(prod.r)
  )

  val expr =
    new Sum(new Prod(new Num(1), new Num(2)), new Num(3))
  println(eval(expr)) // prints 5
}

