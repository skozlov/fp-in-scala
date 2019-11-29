package com.github.skozlov.fpinscala

class FibonacciSpec extends Spec {
	"fibonacci" should "throw IllegalArgumentException if `number` argument is negative" in {
		(the [IllegalArgumentException] thrownBy fibonacci(-1)).getMessage shouldBe "requirement failed: number is negative (-1)"
		(the [IllegalArgumentException] thrownBy fibonacci(-1, (1, 2))).getMessage shouldBe "requirement failed: number is negative (-1)"
	}

	it should "return Fibonacci numbers" in {
		{
			val fibonaccies = LazyList.from(0) map {number => fibonacci(number)}
			(fibonaccies take 8) shouldBe LazyList(0, 1, 1, 2, 3, 5, 8, 13)
		}
		{
			val fibonaccies = LazyList.from(0) map {number => fibonacci(number, (1, 2))}
			(fibonaccies take 6) shouldBe LazyList(1, 2, 3, 5, 8, 13)
		}
	}
}