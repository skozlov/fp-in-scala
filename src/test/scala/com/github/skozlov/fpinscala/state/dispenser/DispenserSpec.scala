package com.github.skozlov.fpinscala.state.dispenser

import com.github.skozlov.fpinscala.Spec
import com.github.skozlov.fpinscala.state.dispenser.Machine.simulate
import com.github.skozlov.fpinscala.state.dispenser.Input._

class DispenserSpec extends Spec {
	private def test(init: Machine, expected: Machine, inputs: List[Input]): Unit = {
		simulate(inputs).run(init) shouldBe ((), expected)
	}

	"inserting a coin" should "unlock a machine if there is any candy left" in {
		val testCoin = test(_, _, List(Coin))
		testCoin(Machine(locked = true, candies = 1, coins = 0), Machine(locked = false, candies = 1, coins = 1))
		testCoin(Machine(locked = true, candies = 1, coins = 1), Machine(locked = false, candies = 1, coins = 2))
		testCoin(Machine(locked = false, candies = 1, coins = 0), Machine(locked = false, candies = 1, coins = 1))
		testCoin(Machine(locked = true, candies = 0, coins = 0), Machine(locked = true, candies = 0, coins = 0))
	}

	"turning the knob" should "dispense candy and lock a machine if it is unlocked" in {
		val testTurn = test(_, _, List(Turn))
		testTurn(Machine(locked = false, candies = 1, coins = 0), Machine(locked = true, candies = 0, coins = 0))
		testTurn(Machine(locked = false, candies = 2, coins = 0), Machine(locked = true, candies = 1, coins = 0))
		testTurn(Machine(locked = false, candies = 1, coins = 1), Machine(locked = true, candies = 0, coins = 1))
		testTurn(Machine(locked = true, candies = 1, coins = 1), Machine(locked = true, candies = 1, coins = 1))
		testTurn(Machine(locked = false, candies = 0, coins = 1), Machine(locked = false, candies = 0, coins = 1))
	}

	"buying a candy" should "work as expected" in {
		test(
			Machine(locked = true, candies = 1, coins = 0),
			Machine(locked = true, candies = 0, coins = 1),
			List(Coin, Turn)
		)
	}
}