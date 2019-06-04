package main

object Main extends App {
    def sum(f: Int => Int, a: Int, b: Int): Int = {
        def loop(a: Int, acc: Int): Int = {
            if (a > b) acc
            else loop(a + 1, acc + f(a))
        }
        loop(a, 0)
    }
    def product(f: Int => Int)(a: Int, b: Int): Int = generalized((x, y) => x * y)(1)(f)(a, b)

    def factorial(a: Int) = product(x => x)(1, a)

    def generalized(f: (Int, Int) => Int)(unit: Int)(g: Int => Int)(a: Int, b: Int): Int = {
      if (a > b) unit
      else f(g(a), generalized(f)(unit)(g)(a + 1, b))
    }

    // summation
    println(generalized((x, y) => x + y)(0)(x => x)(1, 10))

    // multiplication
    println(generalized((x, y) => x * y)(1)(x => x)(1, 5))

    //factorial defined as product defined as generalization
    println(factorial(6))
    
  
}

