package main

object Main extends App {
  val s1 = FunSets.singletonSet(5)
  println(s1(5))
  println(s1(10))
}

object FunSets {
  type Set = Int => Boolean

  def contains(s: Set, elem: Int): Boolean = s(elem)

  def singletonSet(elem: Int): Set = 
    return (x: Int) => elem == x
  
  def union(s: Set, t: Set): Set = 
    return (x: Int) => if (s(x) || t(x)) true else false
}