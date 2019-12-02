package com.github.skozlov.fpinscala

import scala.annotation.tailrec

trait MyList[+A] {
	def drop(n: Int): MyList[A]

	def dropWhile(p: A => Boolean): MyList[A]
}

object MyList {
	case object Nil extends MyList[Nothing] {
		override def drop(n: Int): Nil.type = this

		override def dropWhile(p: Nothing => Boolean): Nil.type = this
	}

	case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A] {
		override def drop(n: Int): MyList[A] = {
			MyList.drop(this, n) // delegating to companion object to avoid non-tail recursion
		}

		override def dropWhile(p: A => Boolean): MyList[A] = {
			MyList.dropWhile(this, p) // delegating to companion object to avoid non-tail recursion
		}
	}

	def apply(): Nil.type = Nil

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
}