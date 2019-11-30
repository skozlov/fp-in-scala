package com.github.skozlov

import scala.annotation.tailrec

package object fpinscala {
	@tailrec
	def fibonacci(number: Int, seed: (Int, Int) = (0, 1)): Int = {
		require(number >= 0, s"number is negative ($number)")
		number match {
			case 0 => seed._1
			case 1 => seed._2
			case _ => fibonacci(number - 1, (seed._2, seed._1 + seed._2))
		}
	}

	def isSorted[A](as: Array[A])(implicit ordering: Ordering[A]): Boolean = {
		val lastIndex = as.length - 1

		@scala.annotation.tailrec
		def loop(startIndex: Int): Boolean = {
			if (startIndex >= lastIndex) true
			else ordering.lteq(as(startIndex), as(startIndex + 1)) && loop(startIndex + 1)
		}

		loop(0)
	}

	//noinspection ScalaUnnecessaryParentheses
	def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
		a => b => f(a, b)
	}
}