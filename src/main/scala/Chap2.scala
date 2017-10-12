object Chap2 {

  def fib(n: Int): Int = {
    if (n == 0)
      0
    else if (n == 1)
      1
    else {
      @annotation.tailrec
      def go(last: Int, current: Int, index: Int): Int = {
        if (index == n)
          last + current
        else
          go(current, last + current, index + 1)
      }

      go(0, 1, 2)
    }
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {

    if (as.length <= 1)
      true
    else
    {
      def loop(n: Int): Boolean = {
        if (ordered(as(n - 1), as(n)))
          if (n == as.length - 1)
            true
          else
            loop(n + 1)
        else
          false
      }

      loop(1)
    }
  }

  def intOrder(a: Int, b: Int): Boolean = a < b

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    println("The fibonacci of 6 is %d".format(fib(6)))
    println("The Array [] isSorted result is %s".format(isSorted(Array(), intOrder)))
    println("The Array [10] isSorted result is %s".format(isSorted(Array(10), intOrder)))
    println("The Array [-1] isSorted result is %s".format(isSorted(Array(-1), intOrder)))
    println("The Array [-1, 0, 1, 2] isSorted result is %s".format(isSorted(Array(-1, 0, 1, 2), intOrder)))
    println("The Array [3, 4, 5, 6] isSorted result is %s".format(isSorted(Array(3, 4, 5, 6), intOrder)))
    println("The Array [6, 5, 4, 3] isSorted result is %s".format(isSorted(Array(6, 5, 4, 3), intOrder)))
  }
}
