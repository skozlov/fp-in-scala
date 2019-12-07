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

	"depth" should "return number of values in the longest path from the root to a leaf" in {
		Nil.depth shouldBe 0
		leaf(1).depth shouldBe 1
		Branch(1, leaf(2), leaf(3)).depth shouldBe 2
		Branch(1, leaf(2), Nil).depth shouldBe 2
		Branch(1, leaf(2), Branch(3, leaf(4), Nil)).depth shouldBe 3
	}

	"map" should "transform each element but keep structure the same" in {
		Tree.empty[Int] map {_ * 2} shouldBe Nil
		leaf(1) map {_ * 2} shouldBe leaf(2)
		Branch(1, leaf(2), Nil) map {_ * 2} shouldBe Branch(2, leaf(4), Nil)
		Branch(1, leaf(2), leaf(3)) map {_ * 2} shouldBe Branch(2, leaf(4), leaf(6))
	}
}