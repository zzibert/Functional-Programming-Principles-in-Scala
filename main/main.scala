package main

object Main extends App {
    def sum(f: Int => Int, a: Int, b: Int): Int = {
        def loop(a: Int, acc: Int): Int = {
            if (a > b) acc
            else loop(a + 1, acc + f(a))
        }
        loop(a, 0)
    }
    def product(f: Int => Int)(a: Int, b: Int): Int = {
      if (a > b) 1
      else f(a) * product(f)(a + 1, b)
    }

    def factorial(a: Int) = product(x => x)(1, a)

    println(factorial(6))
    
  
}

