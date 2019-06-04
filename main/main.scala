package main

object Main extends App {
    def sum(f: Int => Int, a: Int, b: Int): Int = {
        def loop(a: Int, acc: Int): Int = {
            if (a > b) acc
            else loop(a + 1, acc + f(a))
        }
        loop(a, 0)
    }

    println(sum(x => x * x, 0, 4)) 
}

