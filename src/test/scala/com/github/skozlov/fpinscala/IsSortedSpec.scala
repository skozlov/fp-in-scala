package com.github.skozlov.fpinscala

class IsSortedSpec extends Spec {
	"isSorted" should "return true for empty array" in {
		isSorted(Array.emptyIntArray) shouldBe true
	}

	it should "return true for one element array" in {
		isSorted(Array(1)) shouldBe true
	}

	it should "return true for sorted array of several elements" in {
		isSorted(Array(1, 2)) shouldBe true
		isSorted(Array(1, 2, 3)) shouldBe true
		isSorted(Array(1, 1)) shouldBe true
		isSorted(Array(1, 1, 2)) shouldBe true
	}

	it should "return false for array that is not sorted" in {
		isSorted(Array(2, 1)) shouldBe false
		isSorted(Array(2, 3, 1)) shouldBe false
	}
}