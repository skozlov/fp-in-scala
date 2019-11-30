package com.github.skozlov.fpinscala

class CurrySpec extends Spec {
	"curry" should "convert a function f of 2 arguments into a function of 1 argument that partially applies f" in {
		val f = curry((a: Int, b: Int) => a + b)
		val f1 = f(1)
		f1(2) shouldBe 3
		val f2 = f(2)
		f2(2) shouldBe 4
	}

	"uncurry" should "reverse the transformation of curry" in {
		//noinspection ScalaUnnecessaryParentheses
		val uncurried = uncurry((a: Int) => ((b: Int) => a + b))
		uncurried(1, 2) shouldBe 3
		uncurried(2, 2) shouldBe 4
	}
}