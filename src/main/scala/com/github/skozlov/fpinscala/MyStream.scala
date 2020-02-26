package com.github.skozlov.fpinscala

import scala.annotation.tailrec

sealed trait MyStream[+A] {
	import MyStream._

	def foldLeft[B](seed: B)(f: (B, A) => B): B = {
		@tailrec
		def fold(stream: MyStream[A], seed: B): B = stream match {
			case Nil => seed
			case Cons(head, tail) => fold(tail(), f(seed, head()))
		}

		fold(this, seed)
	}

	def toList: List[A] = foldLeft(List.empty[A]){(list, a) => a :: list}.reverse

	def take(n: Int): MyStream[A] = {
		require(n >= 0)

		def take(stream: => MyStream[A], n: Int): MyStream[A] = {
			if (n == 0) {
				Nil
			}
			else stream match {
				case Nil => Nil
				case Cons(head, tail) =>
					lazy val t = take(tail(), n-1)
					Cons(head, () => t)
			}
		}

		take(this, n)
	}

	def drop(n: Int): MyStream[A] = {
		require(n >= 0)

		@tailrec
		def drop(stream: MyStream[A], n: Int): MyStream[A] = {
			if (n == 0) {
				stream
			}
			else stream match {
				case Nil => Nil
				case Cons(_, tail) => drop(tail(), n-1)
			}
		}

		drop(this, n)
	}

	def foldRight[B](seed: => B)(f: (=> A, => B) => B): B = this match {
		case Nil => seed
		case Cons(head, tail) => f(head(), tail().foldRight(seed)(f))
	}

	def scanRight[B](seed: => B)(f: (=> A, => B) => B): MyStream[B] = tails map {_.foldRight(seed)(f)}

	def exists(p: A => Boolean): Boolean = foldRight(false){(a, found) => p(a) || found}

	def forAll(p: A => Boolean): Boolean = foldRight(true){(a, all) => p(a) && all}

	def takeWhile(p: A => Boolean): MyStream[A] = foldRight(empty[A]){(head, tail) =>
		if (p(head)) {
			cons(head, tail)
		}
		else Nil
	}

	def dropWhile(p: A => Boolean): MyStream[A] = this match {
		case Nil => Nil
		case Cons(head, tail) =>
			if (p(head())) {
				tail() dropWhile p
			} else this
	}

	def headOpt: Option[A] = foldRight(Option.empty[A]){(head, _) => Some(head)}

	def flatMap[B](f: A => MyStream[B]): MyStream[B] = foldRight(empty[B]){(head, tail) => concat(f(head), tail)}

	def map[B](f: A => B): MyStream[B] = flatMap {a => cons(f(a), Nil)}

	def filter(p: A => Boolean): MyStream[A] = flatMap {a => if (p(a)) MyStream(a) else Nil}

	def find(p: A => Boolean): Option[A] = filter(p).headOpt

	def zip[B](that: MyStream[B]): MyStream[(Option[A], Option[B])] = (this, that) match {
		case (Cons(headLeft, tailLeft), Cons(headRight, tailRight)) => cons(
			(Some(headLeft()), Some(headRight())),
			tailLeft() zip tailRight()
		)
		case (stream, Nil) => stream map {a => (Some(a), None)}
		case (Nil, stream) => stream map {b => (None, Some(b))}
	}

	def tails: MyStream[MyStream[A]] = this match {
		case Nil => MyStream(Nil)
		case Cons(_, tail) => cons(this, tail().tails)
	}
}

object MyStream {
	case object Nil extends MyStream[Nothing]

	case class Cons[+A](head: () => A, tail: () => MyStream[A]) extends MyStream[A]

	def empty[A]: MyStream[A] = Nil

	def cons[A](head: => A, tail: => MyStream[A]): Cons[A] = {
		lazy val h = head
		lazy val t = tail
		Cons(() => h, () => t)
	}

	def apply[A](elements: A*): MyStream[A] = {
		if (elements.isEmpty) {
			Nil
		}
		else {
			cons(elements.head, MyStream(elements.tail: _*))
		}
	}

	def concat[A](stream1: MyStream[A], stream2: => MyStream[A]): MyStream[A] = stream1.foldRight(stream2){cons(_, _)}

	def unfold[A, S](initState: S)(f: S => Option[(A, S)]): MyStream[A] = {
		f(initState)
			.map{ case (head, nextState) => cons(head, unfold(nextState)(f)) }
			.getOrElse(Nil)
	}

	def generate[A](f: () => A): MyStream[A] = cons(f(), generate(f))

	def const[A](a: => A): MyStream[A] = generate(() => a)

	def arithmeticProgression(start: => BigDecimal, diff: => BigDecimal): MyStream[BigDecimal] = {
		unfold(start){x => Some((x, x + diff))}
	}

	def from(n: Int): MyStream[Int] = arithmeticProgression(1, 1) map {_.toIntExact}

	def fibs(first: => Int, second: => Int): MyStream[Int] = unfold((first, second)){case (a, b) => Some((a, (b, a + b)))}

	@tailrec
	def startsFrom[A](stream: => MyStream[A], prefix: MyStream[A]): Boolean = prefix match {
		case Nil => true
		case Cons(prefixHead, prefixTail) => stream match {
			case Nil => false
			case Cons(head, tail) => head() == prefixHead() && startsFrom(tail(), prefixTail())
		}
	}

	@tailrec
	def contains[A](stream: => MyStream[A], subStream: MyStream[A]): Boolean = {
		startsFrom(stream, subStream) || (stream match {
			case Cons(_, tail) => contains(tail(), subStream)
			case _ => false
		})
	}
}