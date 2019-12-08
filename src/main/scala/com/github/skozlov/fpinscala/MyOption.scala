package com.github.skozlov.fpinscala

import scala.annotation.tailrec

sealed trait MyOption[+A]{
	import MyOption._

	def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
		case None => None
		case Some(value) => f(value)
	}

	def map[B](f: A => B): MyOption[B] = flatMap {a => Some(f(a))}

	def filter(p: A => Boolean): MyOption[A] = this match {
		case Some(value) if p(value) => this
		case _ => None
	}

	def orElse[B >: A](default: => MyOption[B]): MyOption[B] = this match {
		case None => default
		case Some(_) => this
	}

	def getOrElse[B >: A](default: => B): B = this match {
		case None => default
		case Some(value) => value
	}
}

object MyOption{
	case class Some[+A](value: A) extends MyOption[A]

	case object None extends MyOption[Nothing]

	def none[A]: MyOption[A] = None

	def traverse[A, B](as: List[A])(f: A => MyOption[B]): MyOption[List[B]] = {
		@tailrec
		def loop(as: List[A], resultReversed: List[B]): MyOption[List[B]] = as match {
			case Nil => Some(resultReversed.reverse)
			case a :: rest => f(a) match {
				case None => None
				case Some(b) => loop(rest, b :: resultReversed)
			}
		}

		loop(as, Nil)
	}

	def sequence[A](opts: List[MyOption[A]]): MyOption[List[A]] = traverse(opts)(identity)
}