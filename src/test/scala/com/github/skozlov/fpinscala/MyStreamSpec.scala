package com.github.skozlov.fpinscala

import MyStream._

class MyStreamSpec extends Spec {
	"MyStream(...)" should "construct a stream from passed elements" in {
		MyStream() shouldBe Nil
		MyStream(1, 2) match {
			case Cons(head, tail) =>
				head() shouldBe 1
				tail() match {
					case Cons(head, tail) =>
						head() shouldBe 2
						tail() shouldBe Nil
				}
		}
	}

	"cons(head, tail)" should "make head and tail evaluated lazily and only once" in {
		var headEvals = 0
		var tailEvals = 0
		val stream = cons({headEvals = headEvals + 1; 0}, {tailEvals = tailEvals + 1; Nil})
		headEvals shouldBe 0
		tailEvals shouldBe 0
		for (_ <- 1 to 2) {
			stream.head()
			headEvals shouldBe 1
			tailEvals shouldBe 0
		}
		for (_ <- 1 to 2) {
			stream.tail()
			headEvals shouldBe 1
			tailEvals shouldBe 1
		}
	}

	"headOpt" should "return None for Nil" in {
		Nil.headOpt shouldBe None
	}

	"headOpt" should "return Some(firstElement) for Cons" in {
		MyStream(1, 2).headOpt shouldBe Some(1)
	}

	"headOpt" should "be lazy" in {
		var headEvals = 0
		var tailEvals = 0
		var subHeadEvals = 0
		var subTailEvals = 0
		val stream = cons(
			{headEvals = headEvals + 1; 0},
			{tailEvals = tailEvals + 1; cons(
				{subHeadEvals = subHeadEvals + 1; 1},
				{subTailEvals = subTailEvals + 1; Nil}
			)}
		)
		(headEvals, tailEvals, subHeadEvals, subTailEvals) shouldBe (0, 0, 0, 0)

		stream.headOpt shouldBe Some(0)
		(headEvals, tailEvals, subHeadEvals, subTailEvals) shouldBe (1, 0, 0, 0)
	}

	"toList" should "convert this stream into List" in {
		Nil.toList shouldBe scala.Nil
		MyStream(1).toList shouldBe List(1)
		MyStream(1, 2).toList shouldBe List(1, 2)
	}

	"take(n)" should "return first n elements of this stream" in {
		Nil take 0 shouldBe Nil
		Nil take 1 shouldBe Nil
		MyStream(1, 2) take 0 shouldBe Nil
		(MyStream(1, 2) take 1).toList shouldBe List(1)
		(MyStream(1, 2) take 2).toList shouldBe List(1, 2)
		(MyStream(1, 2) take 3).toList shouldBe List(1, 2)
	}

	"take" should "be lazy" in {
		var headEvals = 0
		var tailEvals = 0
		var subHeadEvals = 0
		var subTailEvals = 0
		val stream = cons(
			{headEvals = headEvals + 1; 0},
			{tailEvals = tailEvals + 1; cons(
				{subHeadEvals = subHeadEvals + 1; 1},
				{subTailEvals = subTailEvals + 1; Nil}
			)}
		)
		(headEvals, tailEvals, subHeadEvals, subTailEvals) shouldBe (0, 0, 0, 0)

		stream take 0
		(headEvals, tailEvals, subHeadEvals, subTailEvals) shouldBe (0, 0, 0, 0)

		val first = stream take 1
		(headEvals, tailEvals, subHeadEvals, subTailEvals) shouldBe (0, 0, 0, 0)

		first.toList shouldBe List(0)
		(headEvals, tailEvals, subHeadEvals, subTailEvals) shouldBe (1, 0, 0, 0)
	}

	"drop(n)" should "drop first n elements of this stream" in {
		Nil drop 0 shouldBe Nil
		Nil drop 1 shouldBe Nil
		(MyStream(1, 2) drop 0).toList shouldBe List(1, 2)
		(MyStream(1, 2) drop 1).toList shouldBe List(2)
		MyStream(1, 2) drop 2 shouldBe Nil
		MyStream(1, 2) drop 3 shouldBe Nil
	}

	"takeWhile" should "drop the first element not matching the given predicate and all subsequent elements" in {
		(MyStream(1, 2, 3, 4) takeWhile {_ <= 2}).toList shouldBe List(1, 2)
		(MyStream(1, 2, 3, 4) takeWhile {_ <= 4}).toList shouldBe List(1, 2, 3, 4)
		MyStream(1, 2, 3, 4) takeWhile {_ < 0} shouldBe Nil
		Nil takeWhile {_ => true} shouldBe Nil
		Nil takeWhile {_ => false} shouldBe Nil
	}

	"takeWhile" should "be lazy" in {
		var headEvals = 0
		var tailEvals = 0
		var subHeadEvals = 0
		var subTailEvals = 0
		val stream = cons(
			{headEvals = headEvals + 1; 0},
			{tailEvals = tailEvals + 1; cons(
				{subHeadEvals = subHeadEvals + 1; 1},
				{subTailEvals = subTailEvals + 1; Nil}
			)}
		)
		(headEvals, tailEvals, subHeadEvals, subTailEvals) shouldBe (0, 0, 0, 0)

		stream takeWhile {_ < 0} shouldBe Nil
		(headEvals, tailEvals, subHeadEvals, subTailEvals) shouldBe (1, 0, 0, 0)

		val nonPositive = stream takeWhile {_ <= 0}
		val all = stream takeWhile {_ => true}
		(headEvals, tailEvals, subHeadEvals, subTailEvals) shouldBe (1, 0, 0, 0)

		nonPositive.toList shouldBe List(0)
		(headEvals, tailEvals, subHeadEvals, subTailEvals) shouldBe (1, 1, 1, 0)

		all.toList shouldBe List(0, 1)
		(headEvals, tailEvals, subHeadEvals, subTailEvals) shouldBe (1, 1, 1, 1)
	}

	"dropWhile" should "drop elements while them matching the given predicate" in {
		Nil dropWhile {_ => true} shouldBe Nil
		Nil dropWhile {_ => false} shouldBe Nil
		MyStream(1, 2) dropWhile {_ => true} shouldBe Nil
		(MyStream(1, 2) dropWhile {_ => false}).toList shouldBe List(1, 2)
		(MyStream(1, 2) dropWhile {_ <= 1}).toList shouldBe List(2)
	}

	"dropWhile" should "be lazy" in {
		var headEvals = 0
		var tailEvals = 0
		var subHeadEvals = 0
		var subTailEvals = 0
		val stream = cons(
			{headEvals = headEvals + 1; 0},
			{tailEvals = tailEvals + 1; cons(
				{subHeadEvals = subHeadEvals + 1; 1},
				{subTailEvals = subTailEvals + 1; Nil}
			)}
		)
		(headEvals, tailEvals, subHeadEvals, subTailEvals) shouldBe (0, 0, 0, 0)

		stream dropWhile {_ => false}
		(headEvals, tailEvals, subHeadEvals, subTailEvals) shouldBe (1, 0, 0, 0)

		stream dropWhile {_ <= 0}
		(headEvals, tailEvals, subHeadEvals, subTailEvals) shouldBe (1, 1, 1, 0)
	}

	"exists" should "return true iff an element matches the given predicate" in {
		Nil exists {_ => true} shouldBe false
		MyStream(1, 2) exists {_ >= 2} shouldBe true
		MyStream(1, 2) exists {_ >= 3} shouldBe false
	}

	"exists" should "be lazy" in {
		var headEvals = 0
		var tailEvals = 0
		var subHeadEvals = 0
		var subTailEvals = 0
		val stream = cons(
			{headEvals = headEvals + 1; 0},
			{tailEvals = tailEvals + 1; cons(
				{subHeadEvals = subHeadEvals + 1; 1},
				{subTailEvals = subTailEvals + 1; Nil}
			)}
		)
		(headEvals, tailEvals, subHeadEvals, subTailEvals) shouldBe (0, 0, 0, 0)

		stream exists {_ == 0} shouldBe true
		(headEvals, tailEvals, subHeadEvals, subTailEvals) shouldBe (1, 0, 0, 0)
	}

	"forAll" should "return true iff each elements match the given predicate" in {
		Nil forAll {_ => false} shouldBe true
		MyStream(1, 2) forAll {_ <= 2} shouldBe true
		MyStream(1, 2) forAll {_ <= 1} shouldBe false
	}

	"forAll" should "be lazy" in {
		var headEvals = 0
		var tailEvals = 0
		var subHeadEvals = 0
		var subTailEvals = 0
		val stream = cons(
			{headEvals = headEvals + 1; 0},
			{tailEvals = tailEvals + 1; cons(
				{subHeadEvals = subHeadEvals + 1; 1},
				{subTailEvals = subTailEvals + 1; Nil}
			)}
		)
		(headEvals, tailEvals, subHeadEvals, subTailEvals) shouldBe (0, 0, 0, 0)

		stream forAll {_ => false} shouldBe false
		(headEvals, tailEvals, subHeadEvals, subTailEvals) shouldBe (1, 0, 0, 0)
	}

	"concat" should "concatenate two streams" in {
		concat(Nil, Nil) shouldBe Nil
		concat(Nil, MyStream(1, 2)).toList shouldBe List(1, 2)
		concat(MyStream(1, 2), Nil).toList shouldBe List(1, 2)
		concat(MyStream(1, 2), MyStream(3, 4)).toList shouldBe List(1, 2, 3, 4)
	}

	"concat" should "be lazy" in {
		var head1Evals = 0
		var tail1Evals = 0
		var stream2Evals = 0
		concat(
			cons(
				{head1Evals = head1Evals + 1; 0},
				{tail1Evals = tail1Evals + 1; Nil}
			),
			{stream2Evals = stream2Evals + 1; MyStream(2, 3)}
		)
		(head1Evals, tail1Evals, stream2Evals) shouldBe (0, 0, 0)
	}

	"flatMap" should "map each element to a stream and concatenate those streams" in {
		(MyStream(1, 2, 3) flatMap {x => if (x % 2 == 0) Nil else MyStream(x, x + 10)}).toList shouldBe List(1, 11, 3, 13)
	}

	"flatMap" should "be lazy" in {
		val sourceEvals = Array.fill(2)(0)
		val targetEvals = Array.fill(4)(0)
		val source = cons(
			{sourceEvals(0) = sourceEvals(0) + 1; 1},
			cons(
				{sourceEvals(1) = sourceEvals(1) + 1; 2},
				Nil
			)
		)
		(sourceEvals.toList, targetEvals.toList) shouldBe (List(0, 0), List(0, 0, 0, 0))

		val target = source flatMap {
			case 1 => cons(
				{targetEvals(0) = targetEvals(0) + 1; 3},
				cons(
					{targetEvals(1) = targetEvals(1) + 1; 4},
					Nil
				)
			)
			case 2 => cons(
				{targetEvals(2) = targetEvals(2) + 1; 5},
				cons(
					{targetEvals(3) = targetEvals(3) + 1; 6},
					Nil
				)
			)
		}
		(sourceEvals.toList, targetEvals.toList) shouldBe (List(1, 0), List(0, 0, 0, 0))

		(target take 1).toList shouldBe List(3)
		(sourceEvals.toList, targetEvals.toList) shouldBe (List(1, 0), List(1, 0, 0, 0))

		(target take 2).toList shouldBe List(3, 4)
		(sourceEvals.toList, targetEvals.toList) shouldBe (List(1, 0), List(1, 1, 0, 0))

		(target take 3).toList shouldBe List(3, 4, 5)
		(sourceEvals.toList, targetEvals.toList) shouldBe (List(1, 1), List(1, 1, 1, 0))
	}

	"map" should "map each element" in {
		(MyStream(1, 2) map {_ * 2}).toList shouldBe List(2, 4)
	}

	"map" should "be lazy" in {
		val sourceEvals = Array.fill(2)(0)
		val targetEvals = Array.fill(2)(0)
		val source = cons(
			{sourceEvals(0) = sourceEvals(0) + 1; 0},
			cons(
				{sourceEvals(1) = sourceEvals(1) + 1; 1},
				Nil
			)
		)
		(sourceEvals.toList, targetEvals.toList) shouldBe (List(0, 0), List(0, 0))

		val target = source map {i =>
			targetEvals(i) = targetEvals(i) + 1
			i + 10
		}
		(sourceEvals.toList, targetEvals.toList) shouldBe (List(1, 0), List(0, 0))

		(target take 1).toList shouldBe List(10)
		(sourceEvals.toList, targetEvals.toList) shouldBe (List(1, 0), List(1, 0))

		target.toList shouldBe List(10, 11)
		(sourceEvals.toList, targetEvals.toList) shouldBe (List(1, 1), List(1, 1))
	}

	"filter" should "drop all elements not matching the given predicate" in {
		Nil filter {_ => true} shouldBe Nil
		Nil filter {_ => false} shouldBe Nil
		MyStream(1, 2, 3) filter {_ => false} shouldBe Nil
		(MyStream(1, 2, 3) filter {_ % 2 != 0}).toList shouldBe List(1, 3)
	}

	"filter" should "be lazy" in {
		val evals = Array.fill(4)(0)
		val source = cons(
			{evals(0) = evals(0) + 1; 0},
			cons(
				{evals(1) = evals(1) + 1; 1},
				cons(
					{evals(2) = evals(2) + 1; 2},
					cons(
						{evals(3) = evals(3) + 1; 3},
						Nil
					)
				)
			)
		)
		evals.toList shouldBe List(0, 0, 0, 0)

		val target = source filter {_ != 1}
		evals.toList shouldBe List(1, 0, 0, 0)

		(target take 1).toList shouldBe List(0)
		evals.toList shouldBe List(1, 0, 0, 0)

		(target take 2).toList shouldBe List(0, 2)
		evals.toList shouldBe List(1, 1, 1, 0)
	}

	"find" should "find the first element matching the predicate if any" in {
		MyStream(1, 2, 3) find {_ >= 2} shouldBe Some(2)
		MyStream(1, 2, 3) find {_ >= 4} shouldBe None
		Nil find {_ => true} shouldBe None
	}

	"find" should "be lazy" in {
		val evals = Array.fill(3)(0)
		val stream = cons(
			{evals(0) = evals(0) + 1; 0},
			cons(
				{evals(1) = evals(1) + 1; 1},
				cons(
					{evals(2) = evals(2) + 1; 2},
					Nil
				)
			)
		)
		evals.toList shouldBe List(0, 0, 0)

		stream find {_ == 1} shouldBe Some(1)
		evals.toList shouldBe List(1, 1, 0)
	}

	"const(x)" should "create an infinite stream of x" in {
		(const(0) take 3).toList shouldBe List(0, 0, 0)
	}

	"arithmeticProgression(a, d)" should "create an arithmetic progression from a with difference of d" in {
		(arithmeticProgression(1, 1) take 10).toList.sum shouldBe 55
	}

	"fibs" should "create a Fibonacci sequence" in {
		(fibs(0, 1) take 8).toList shouldBe List(0, 1, 1, 2, 3, 5, 8, 13)
	}

	"unfold" should "stop generating values when returning None" in {
		unfold(1){x => if(x <= 3) Some((x, x + 1)) else None}.toList shouldBe List(1, 2, 3)
	}

	"zip" should "take pairs of elements from two streams in sync until both streams are ended" in {
		Nil zip Nil shouldBe Nil
		(MyStream(1, 2, 3) zip MyStream("1")).toList shouldBe List((Some(1), Some("1")), (Some(2), None), (Some(3), None))
		(MyStream("1") zip MyStream(1, 2, 3)).toList shouldBe List((Some("1"), Some(1)), (None, Some(2)), (None, Some(3)))
		(from(1) zip MyStream("1") take 4).toList shouldBe List((Some(1), Some("1")), (Some(2), None), (Some(3), None), (Some(4), None))
	}

	"zip" should "be lazy" in {
		val leftEvals = Array.fill(2)(0)
		val rightEvals = Array.fill(2)(0)
		val left = cons(
			{leftEvals(0) = leftEvals(0) + 1; 0},
			cons(
				{leftEvals(1) = leftEvals(1) + 1; 1},
				Nil
			)
		)
		val right = cons(
			{rightEvals(0) = rightEvals(0) + 1; "0"},
			cons(
				{rightEvals(1) = rightEvals(1) + 1; "1"},
				Nil
			)
		)
		val zipped = left zip right
		(leftEvals.toList, rightEvals.toList) shouldBe (List(0, 0), List(0, 0))

		zipped.headOpt shouldBe Some((Some(0), Some("0")))
		(leftEvals.toList, rightEvals.toList) shouldBe (List(1, 0), List(1, 0))

		zipped.toList shouldBe List((Some(0), Some("0")), (Some(1), Some("1")))
		(leftEvals.toList, rightEvals.toList) shouldBe (List(1, 1), List(1, 1))
	}

	"startsFrom(a, b)" should "return true iff stream b is a prefix of stream a" in {
		startsFrom(Nil, Nil) shouldBe true
		startsFrom(MyStream(1), Nil) shouldBe true
		startsFrom(Nil, MyStream(1)) shouldBe false
		startsFrom(MyStream(1, 2, 3), MyStream(1, 2, 3)) shouldBe true
		startsFrom(MyStream(1, 2, 3), MyStream(1, 2)) shouldBe true
		startsFrom(MyStream(1, 2, 3), MyStream(2, 3)) shouldBe false
		startsFrom(MyStream(1, 2), MyStream(1, 2, 3)) shouldBe false
		startsFrom(from(1), MyStream(1, 2)) shouldBe true
		startsFrom(from(1), MyStream(1, 2, 4)) shouldBe false
		startsFrom(MyStream(1, 2), from(1)) shouldBe false
		startsFrom(arithmeticProgression(1, 1), arithmeticProgression(1, 2)) shouldBe false
	}

	"contains(a, b)" should "return true iff stream a contains stream b as a subsequence" in {
		contains(Nil, Nil) shouldBe true
		contains(MyStream(1), Nil) shouldBe true
		contains(Nil, MyStream(1)) shouldBe false
		contains(MyStream(1, 2), MyStream(1, 2)) shouldBe true
		contains(MyStream(1, 2, 3, 4), MyStream(2, 3)) shouldBe true
		contains(MyStream(1, 2, 3, 4), MyStream(1, 3)) shouldBe false
		contains(from(1), MyStream(2, 3)) shouldBe true
		contains(MyStream(2, 3), from(2)) shouldBe false
	}

	"tails" should "return stream of all tails starting from this stream itself" in {
		Nil.tails.toList shouldBe List(Nil)
		MyStream(1, 2).tails.toList map {_.toList} shouldBe List(List(1, 2), List(2), List())
		(from(1).tails take 3).toList map {tail => (tail take 3).toList} shouldBe
			List(List(1, 2, 3), List(2, 3, 4), List(3, 4, 5))
	}

	"scanRight" should "be like foldRight, but return a stream of intermediate results" in {
		MyStream.empty[Int].scanRight(0){_ + _}.toList shouldBe List(0)
		MyStream(1, 2, 3).scanRight(0){_ + _}.toList shouldBe List(6, 5, 3, 0)
	}
}