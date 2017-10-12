package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(n) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match {
    case Leaf(a) => l(a)
    case Branch(x, y) => b(fold(x)(l)(b), fold(y)(l)(b))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(_ + _ + 1)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)((x:Int) => x)((x:Int, y:Int) => x max y)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)(1 + _ max _)

  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    def leafFunc[A](a: A): Tree[B] = Leaf(f(a))
    def branchFunc[B](x: Tree[B], y: Tree[B]): Tree[B] = Branch(x, y)
    fold(t)(leafFunc)(branchFunc)
  }
}
