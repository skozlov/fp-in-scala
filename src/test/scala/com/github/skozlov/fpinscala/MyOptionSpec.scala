package com.github.skozlov.fpinscala

import MyOption._

class MyOptionSpec extends Spec {
	"flatMap" should "transform and filter a value" in {
		Some(1) flatMap {x => if (x % 2 == 0) Some(x * 2) else None} shouldBe None
		Some(2) flatMap {x => if (x % 2 == 0) Some(x * 2) else None} shouldBe Some(4)
		none[Int] flatMap {x => if (x % 2 == 0) Some(x * 2) else None} shouldBe None
	}

	"map" should "transform a value" in {
		Some(1) map {_ * 2} shouldBe Some(2)
		none[Int] map {_ * 2} shouldBe None
	}

	"filter" should "filter a value" in {
		Some(2) filter {_ % 2 == 0} shouldBe Some(2)
		Some(1) filter {_ % 2 == 0} shouldBe None
		none[Int] filter {_ % 2 == 0} shouldBe None
	}

	"orElse" should "return argument for None" in {
		Some(1) orElse Some(2) shouldBe Some(1)
		None orElse Some(2) shouldBe Some(2)
	}

	"getOrElse" should "return argument for None" in {
		Some(1) getOrElse 2 shouldBe 1
		None getOrElse 2 shouldBe 2
	}

	"sequence" should "return None if the argument contains None, otherwise the list flatten" in {
		sequence(Nil) shouldBe Some(Nil)
		sequence(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1, 2, 3))
		sequence(List(Some(1), None, Some(3))) shouldBe None
	}
}