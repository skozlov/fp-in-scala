package com.github.skozlov.fpinscala.random

import com.github.skozlov.fpinscala.Spec
import com.github.skozlov.fpinscala.state.State

class RandomSpec extends Spec {
	"nextInt" should "work as expected" in {
		val s0 = SimpleRandomGenerator(0)
		val (i0, s1) = s0.nextInt
		i0 shouldBe 0
		val (i1, s2) = s1.nextInt
		i1 shouldBe 4232237
		s2.nextInt._1 shouldBe 178803790
		s2.nextInt._1 shouldBe 178803790
	}

	"map" should "transform each result" in {
		(Random.ints(3) map {_ map {_  + 1}}).run(SimpleRandomGenerator(0))._1 shouldBe List(1, 4232238, 178803791)
	}

	"map2" should "combine two randoms" in {
		val (pair, finalState) = Random{_.nextInt}.map2(Random{_.nextInt}){(_, _)}.run(SimpleRandomGenerator(0))
		pair shouldBe (0, 4232237)
		finalState.nextInt._1 shouldBe 178803790
	}

	"sequence" should "transform a list of randoms to random lists" in {
		val (list, finalState) = State.sequence[RandomGenerator, Int](List(Random{_.nextInt}, Random{_.nextInt}))
			.run(SimpleRandomGenerator(0))
		list shouldBe List(0, 4232237)
		finalState.nextInt._1 shouldBe 178803790
	}
}