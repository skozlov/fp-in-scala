package com.github.skozlov.fpinscala

sealed trait Tree[+A]{
	def size: Int

	def depth: Int
}

object Tree {
	case object Nil extends Tree[Nothing] {
		override def size: Int = 0

		override def depth: Int = 0
	}

	case class Branch[+A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A] {
		override def size: Int = left.size + right.size + 1

		override def depth: Int = 1 + Math.max(left.depth, right.depth)
	}

	def empty[A]: Tree[A] = Nil

	def leaf[A](value: A): Branch[A] = Branch(value, Nil, Nil)

	def max[A](tree: Tree[A])(implicit ordering: Ordering[A]): Option[A] = tree match {
		case Nil => None
		case Branch(value, left, right) => Some((value :: List(max(left), max(right)).flatten).max)
	}

	def max[A](branch: Branch[A])(implicit ordering: Ordering[A]): A = max(branch.asInstanceOf[Tree[A]]).get
}