object Functional {

  // we can define functions inside functions for nice recursion
  // here, we define factorial with a loopy recursive fxn inside
  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  // now here we print a formatted factorial statement thing
  def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  // we can also define something like the fib sequence like this
  def fib(n: Int): Int = {
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)

    loop(n, 0, 1)
  }

  // now we wanna print this too. We can make a more general print function
  // like so
  // This takes a function name, an int, and a function
  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  // let's see some polymorphism!
  // Find first element x of type A in an array of As
  // In english, this is:
  // polyFindFirst[generic A](
  //    arr: array of type A, p: equality fxn for value we're looking for
  // )
  def polyFindFirst[A](arr: Array[A], p: A => Boolean): Int = {
    def loop(n: Int): Int =
      if (n >= arr.length) -1
      else if (p(arr(n))) n
      else loop(n + 1)

    loop(0)
  }

  // function to check if array of As is sorted given a > fxn
  def isSorted[A](arr: Array[A], gt: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean =
      if (n >= arr.length - 1) true
      else if (gt(arr(n), arr(n+1))) false
      else loop(n+1)

    loop(0)
  }

  def formatArr[A](arr: Array[A]): String = {
    def loop(n: Int): String =
      if (n >= arr.length) ""
      else {
        val msg = "%s, %s"
        msg.format(arr(n).toString, loop(n+1))
      }

    loop(0)
  }

  def formatSort[A](arr: Array[A], gt: (A, A) => Boolean): String = {
    val msg = "%s is sorted: %b"
    msg.format(formatArr(arr), isSorted(arr, gt))
  }

  def main(args: Array[String]): Unit = {
    // here we see some examples of the function passing being done
    println(formatFactorial(5))
    println(formatResult("factorial", 5, factorial))
    var i = 0;
    for (i <- 1 to 10) {
      println(formatResult("fib(???)", i, fib))
    }

    // we can even have anonymous functions
    // here are 4 different ways of defining them
    println(formatResult("increment0", 5, (x: Int) => x + 1))
    println(formatResult("increment1", 5, (x) => x + 1))
    println(formatResult("increment2", 5, x => x + 1))
    println(formatResult("increment3", 5, _ + 1))

    // here we see polyFindFirst being used for arrays of ints
    val arr0 = Array(1, 2, 3, 4)
    // find the index where 3 appears
    println(polyFindFirst(arr0, (x: Int) => x == 3))
    // find the index where 5 appears
    println(polyFindFirst(arr0, (x: Int) => x == 5))

    val arr1 = Array(1, 4, 2, 3)
    println(formatSort(arr0, (x: Int, y: Int) => x > y))
    println(formatSort(arr1, (x: Int, y: Int) => x > y))

    // heres some string stuff
    var arr2 = Array("a", "b", "c", "d")
    println(formatSort(arr2, (x: String, y: String) => x > y))
  }

}














// beans
