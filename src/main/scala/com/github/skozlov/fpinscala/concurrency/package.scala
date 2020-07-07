package com.github.skozlov.fpinscala

import com.github.skozlov.fpinscala.concurrency.Par.{fork, map2}

package object concurrency {
	def sum(ints: IndexedSeq[Int]): Par[Int] = {
		if (ints.size <= 1) {
			Par.unit(ints.headOption getOrElse 0)
		}
		else {
			val (left, right) = ints splitAt (ints.length / 2)
			map2(fork(sum(left)), fork(sum(right))){_ + _}
		}
	}
}
