package com.github.skozlov.fpinscala

class ComposeSpec extends Spec {
	"compose" should "feed the output of one function to the input of another function" in {
		val f: Int => Int = _ * 2
		val g: Int => String = _.toString
		val composed = compose(f, g)
		composed(3) shouldBe "6"
	}
}