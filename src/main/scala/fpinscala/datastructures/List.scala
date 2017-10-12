package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead on empty list")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if(n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => l
  }

//  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
//    case Nil => a2
//    case Cons(h, t) => Cons(h, append(t, a2))
//  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => List(buf.toList: _*)
      case Cons(h, t) => buf += h; go(t)
    }
    go(l)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def foldLeftViaFoldRight[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, y) => 1 + y)

  def sumFoldLeft(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def productFoldLeft(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)

  def lengthFoldLeft[A](as: List[A]): Int =
    foldLeft(as, 0)((x, _) => x + 1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil:List[A])((x, y) => Cons(y, x))

  def append[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])((a, b) => append(a, b))

  def plusOne(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((a, b) => Cons(a + 1, b))

  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((a, b) => Cons(a.toString, b))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil:List[B])((a, b) => Cons(f(a), b))

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)((a) => if(f(a)) List(a) else Nil)

  def addPairwise(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addPairwise(xs, ys))
  }

  def zipWith[A,B,C](l: List[A], r: List[B])(f: (A,B) => C): List[C] = (l, r) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y), zipWith(xs, ys)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def check(x: List[A], y: List[A]): Boolean = (x,y) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(a, as), Cons(b, bs)) if a == b => check(as, bs)
      case _ => false
    }

    def loop(x: List[A]): Boolean = x match {
      case Nil => check(x, sub)
      case Cons(a, as) => if(check(x, sub)) true else loop(as)
    }

    loop(sup)
  }

  def main(args: Array[String]): Unit = {
    val finalVal = hasSubsequence(List(1,2,3,4), List(3, 5))
    println("Answer is: " + finalVal)
  }
}
