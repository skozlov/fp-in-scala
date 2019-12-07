package com.github.skozlov.fpinscala

sealed trait Tree[+A]{
	def size: Int
}

object Tree {
	case object Nil extends Tree[Nothing] {
		override def size: Int = 0
	}

	case class Branch[+A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A] {
		override def size: Int = left.size + right.size + 1
	}

	def leaf[A](value: A): Branch[A] = Branch(value, Nil, Nil)
}