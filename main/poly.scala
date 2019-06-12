package main

object week4 {
  trait List[T] {
  def isEmpty: Boolean
  def head: T 
  def tail: List[T]
  }

  class Cons[T](val head: T, val tail: List[T]) extends List[T] {
    def isEmpty = false
  }

  class Nil[T]() extends List[T] {
    def isEmpty = true
    def head = throw NoSuchElementException("Nil.head")
    def tail = throw NoSuchElementException("Nil.tail")
  }
}


