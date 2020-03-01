package com.github.skozlov.fpinscala.state.dispenser

import com.github.skozlov.fpinscala.state.State

case class Machine(locked: Boolean, candies: Int, coins: Int){
	require(candies >= 0)
	require(coins >= 0)
}

object Machine {
	def simulate(inputs: List[Input]): State[Machine, Unit] = {
		import Input._
		State.modify(inputs map { input =>
			machine: Machine => {
				if (machine.candies == 0) {
					machine
				} else input match {
					case Coin => machine.copy(locked = false, coins = machine.coins + 1)
					case Turn =>
						if (machine.locked) {
							machine
						} else {
							machine.copy(locked = true, candies = machine.candies - 1)
						}
				}
			}
		})
	}
}