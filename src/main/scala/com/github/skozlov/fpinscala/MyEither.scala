package com.github.skozlov.fpinscala

import scala.annotation.tailrec

sealed trait MyEither[+E, +A] {
	import MyEither._

	def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = this match {
		case right @ Right(_) => right
		case _ => b
	}

	def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = this match {
		case Right(value) => f(value)
		case left @ Left(_) => left
	}

	def map[B](f: A => B): MyEither[E, B] = flatMap {a => Right(f(a))}

	def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] = flatMap {a => b map {b => f(a, b)}}
}

object MyEither {
	case class Left[+E](value: E) extends MyEither[E, Nothing]

	case class Right[+A](value: A) extends MyEither[Nothing, A]

	def traverse[E, A, B](as: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] = {
		@tailrec
		def loop(as: List[A], resultReversed: List[B]): MyEither[E, List[B]] = as match {
			case Nil => Right(resultReversed.reverse)
			case a :: rest => f(a) match {
				case left @ Left(_) => left
				case Right(b) => loop(rest, b :: resultReversed)
			}
		}

		loop(as, Nil)
	}

	def sequence[E, A, B](es: List[MyEither[E, A]]): MyEither[E, List[A]] = traverse(es)(identity)
}