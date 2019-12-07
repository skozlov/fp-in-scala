package com.github.skozlov.fpinscala

import Tree._

class TreeSpec extends Spec {
	"size" should "return number of values" in {
		Nil.size shouldBe 0
		leaf(0).size shouldBe 1
		Branch(0, leaf(1), leaf(2)).size shouldBe 3
		Branch(0, leaf(1), Branch(2, leaf(3), Nil)).size shouldBe 4
	}

	"max" should "return max value" in {
		max(Tree.empty[Int]) shouldBe None
		max(leaf(0)) shouldBe 0
		max(Branch(0, leaf(1), leaf(2))) shouldBe 2
		max(Branch(2, leaf(0), leaf(1))) shouldBe 2
	}
}