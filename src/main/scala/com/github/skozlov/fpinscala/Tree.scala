package com.github.skozlov.fpinscala

import scala.annotation.tailrec

sealed trait Tree[+A]{
	import Tree._

	def size: Int = fold(0){(_, leftSize, rightSize) => 1 + leftSize + rightSize}

	def depth: Int = fold(0){(_, leftDepth, rightDepth) => 1 + Math.max(leftDepth, rightDepth)}

	def fold[B](seed: B)(f: (A, B, B) => B): B = {
		sealed trait State

		object State{
			case class Start(tree: Tree[A]) extends State
			case class LeftFolded(tree: Branch[A], leftResult: B) extends State
			case class Folded(result: B) extends State
		}

		import State._

		@tailrec
		def fold(pathToRoot: List[State]): B = pathToRoot.head match {
			case Start(Nil) => fold(Folded(seed) :: pathToRoot.tail)
			case Start(Branch(_, left, _)) => fold(Start(left) :: pathToRoot)
			case LeftFolded(Branch(_, _, right), _) => fold(Start(right) :: pathToRoot)
			case Folded(result) =>
				val parents = pathToRoot.tail
				if (parents.isEmpty) {
					result
				} else {
					(parents.head: @unchecked) match {
						case Start(branch @ Branch(_, _, _)) => fold(LeftFolded(branch, result) :: parents.tail)
						case LeftFolded(Branch(value, _, _), leftResult) =>
							fold(Folded(f(value, leftResult, result)) :: parents.tail)
					}
				}
		}

		fold(List(Start(this)))
	}

	def map[B](f: A => B): Tree[B] = fold(empty[B]){(value, left, right) => Branch(f(value), left, right)}
}

object Tree {
	case object Nil extends Tree[Nothing]

	case class Branch[+A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

	def empty[A]: Tree[A] = Nil

	def leaf[A](value: A): Branch[A] = Branch(value, Nil, Nil)

	def max[A](tree: Tree[A])(implicit ordering: Ordering[A]): Option[A] = tree.fold(Option.empty[A]){
		(value, maxLeft, maxRight) => Some((value :: List(maxLeft, maxRight).flatten).max)
	}

	def max[A](branch: Branch[A])(implicit ordering: Ordering[A]): A = max(branch.asInstanceOf[Tree[A]]).get
}