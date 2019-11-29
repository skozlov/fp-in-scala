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
}