package main

object Main extends App {
  val x = new Rational(70, 42)
  val y = new Rational(6, 3)
  val z = new Rational(4, 3)
  

  println(x < y)
  println(x < z)
  println(new Rational(2))
}

class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1)
  private def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a % b)
  private val g = gcd(x, y)
  def numer = x / g
  def denom = y / g
  def + (that: Rational) = 
    new Rational(
        numer * that.denom + that.numer * denom,
        denom * that.denom
    )
  def unary_- = new Rational(-numer, denom)
  def - (that: Rational) = this + -that
  def < (that: Rational) =  ((numer / denom) < (that.numer /that.denom))
  def max(that: Rational) = if (this < that) that else this
  
  override def toString = numer + "/" + denom
}

