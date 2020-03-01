package com.github.skozlov.fpinscala.random

import com.github.skozlov.fpinscala.state
import com.github.skozlov.fpinscala.state.State

object Random {
	def apply[A](run: RandomGenerator => (A, RandomGenerator)): Random[A] = State(run)

	def ints(count: Int): Random[List[Int]] = State.sequence(List.fill(count){state.State{_.nextInt}})
}