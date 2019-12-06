package com.github.skozlov.fpinscala

import scala.annotation.tailrec

trait MyList[+A] {
	import MyList._

	def isEmpty: Boolean

	def drop(n: Int): MyList[A]

	def dropWhile(p: A => Boolean): MyList[A]

	def init: MyList[A]

	def foldLeft[B](seed: B)(f: (B, A) => B): B = MyList.foldLeft(this, seed)(f) // delegating to companion object to avoid non-tail recursion

	def foldRight[B](seed: B)(f: (A, B) => B): B = reverse.foldLeft(seed){(b, a) => f(a, b)}

	def reverse: MyList[A] = foldLeft(MyList.empty[A]){(result, a) => Cons(a, result)}

	def append[B >: A](bs: MyList[B]): MyList[B] = foldRight(bs){Cons(_, _)}
}

object MyList {
	case object Nil extends MyList[Nothing] {
		override def isEmpty: Boolean = true

		override def drop(n: Int): Nil.type = this

		override def dropWhile(p: Nothing => Boolean): Nil.type = this

		override def init: Nil.type = this
	}

	case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A] {
		override def isEmpty: Boolean = false

		override def drop(n: Int): MyList[A] = {
			MyList.drop(this, n) // delegating to companion object to avoid non-tail recursion
		}

		override def dropWhile(p: A => Boolean): MyList[A] = {
			MyList.dropWhile(this, p) // delegating to companion object to avoid non-tail recursion
		}

		override def init: MyList[A] = {
			if (tail.isEmpty) Nil else Cons(head, tail.init)
		}
	}

	def apply(): Nil.type = Nil

	def empty[A]: MyList[A] = Nil

	def apply[A](head: A, tail: A*): Cons[A] = Cons(head, MyList(tail.toList))

	def apply[A](as: List[A]): MyList[A] = as match {
		case head :: tail => Cons(head, MyList(tail))
		case _ => Nil
	}

	@tailrec
	def drop[A](list: MyList[A], n: Int): MyList[A] = (list, n) match {
		case (Cons(_, tail), n) if n > 0 => drop(tail, n - 1)
		case _ => list
	}

	@tailrec
	def dropWhile[A](list: MyList[A], p: A => Boolean): MyList[A] = list match {
		case Cons(head, tail) if p(head) => dropWhile(tail, p)
		case _ => list
	}

	@tailrec
	def foldLeft[A, B](list: MyList[A], seed: B)(f: (B, A) => B): B = list match {
		case Cons(head, tail) => foldLeft(tail, f(seed, head))(f)
		case _ => seed
	}
}