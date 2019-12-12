package com.github.skozlov.fpinscala

import MyEither._

class MyEitherSpec extends Spec {
	"orElse" should "return `this` if it is right" in {
		Right(0) orElse Left(1) shouldBe Right(0)
		Right(0) orElse Right(1) shouldBe Right(0)
	}

	it should "return argument if `this` is left" in {
		Left(0) orElse Left(1) shouldBe Left(1)
		Left(0) orElse Right(1) shouldBe Right(1)
	}

	"flatMap" should "return `this` if it is left" in {
		Left(0) flatMap {_ => Left(1)} shouldBe Left(0)
		Left(0) flatMap {_ => Right(1)} shouldBe Left(0)
	}

	it should "return result of the argument function if `this` is right" in {
		Right(1) flatMap {x => Left(x * 2)} shouldBe Left(2)
		Right(1) flatMap {x => Right(x * 2)} shouldBe Right(2)
	}

	"map" should "return `this` if it is left" in {
		Left(0) map {_ => 1} shouldBe Left(0)
	}

	it should "transform value for right" in {
		Right(1) map {_ * 2} shouldBe Right(2)
	}

	"map2" should "return `this` if it is left" in {
		Left(0).map2(Left(1)){(_, _: Nothing) => 2} shouldBe Left(0)
		Left(0).map2(Right(1)){(_, _) => 2} shouldBe Left(0)
	}

	it should "return 1st argument if it is left and `this` is right" in {
		Right(0).map2(Left(1)){(_, _: Nothing) => 2} shouldBe Left(1)
	}

	it should "return result of function if `this` and the 1st argument are right" in {
		Right(1).map2(Right(2)){_ + _} shouldBe Right(3)
	}

	"sequence" should "return all values as a list wrapped into Right if the input list does not contain Lefts" in {
		sequence(Nil) shouldBe Right(Nil)
		sequence(List(Right(1), Right(2), Right(3))) shouldBe Right(List(1, 2, 3))
	}

	it should "return first Left if any" in {
		sequence(List(Right(1), Left(2), Left(3))) shouldBe Left(2)
	}
}