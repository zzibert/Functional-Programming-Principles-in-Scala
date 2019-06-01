object ScalaTutorial {
    def main(args: Array[String]) {
        
    }

    def factorial(x: Int): Int =
      if (x == 0) 1 else x * factorial(x - 1)

    def tailFactorial(n: Int): Int = {
      def loop(acc: Int, n: Int): Int =
        if (n == 0) acc
        else loop(acc * n, n - 1)
      loop(1, n)
    }

    println(tailFactorial(5))
}