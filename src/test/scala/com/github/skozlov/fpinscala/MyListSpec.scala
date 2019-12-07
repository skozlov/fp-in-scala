package com.github.skozlov.fpinscala

import com.github.skozlov.fpinscala.MyList.hasSubsequence

class MyListSpec extends Spec {
	"apply" should "return Nil if no arguments passed" in {
		val list: MyList.Nil.type = MyList()
		list shouldBe MyList.Nil
	}

	it should "return Cons if arguments passed" in {
		{
			val list: MyList.Cons[Int] = MyList(1)
			list shouldBe MyList.Cons(1, MyList.Nil)
		}
		{
			val list: MyList.Cons[Int] = MyList(1, 2)
			list shouldBe MyList.Cons(1, MyList.Cons(2, MyList.Nil))
		}
	}

	"drop" should "return the same list if n = 0" in {
		val lists = List(MyList(), MyList(1), MyList(1, 2, 3))
		lists foreach { list =>
			list drop 0 should be theSameInstanceAs list
		}
	}

	it should "delete first n elements if there are" in {
		MyList(1, 2, 3) drop 2 shouldBe MyList(3)
		MyList(1, 2, 3) drop 3 shouldBe MyList()
	}

	it should "return empty list if n is more than the size of the list" in {
		MyList() drop 1 shouldBe MyList()
		MyList(1) drop 2 shouldBe MyList()
		MyList(1, 2, 3) drop 4 shouldBe MyList()
	}

	"dropWhile" should "return Nil for Nil" in {
		MyList() dropWhile {_ => true} shouldBe MyList()
		MyList() dropWhile {_ => false} shouldBe MyList()
	}

	it should "return the same list if the predicate does not hold for the head" in {
		val list = MyList(1, 2, 3)
		list dropWhile {_ >= 2} should be theSameInstanceAs list
	}

	it should "return empty list if the predicate holds for each element" in {
		MyList(1, 2, 3) dropWhile {_ < 4} shouldBe MyList()
	}

	it should "drop first elements until the predicate returns false" in {
		MyList(1, 2, 3, 4) dropWhile {_ < 3} shouldBe MyList(3, 4)
	}

	"init" should "return all but last element" in {
		MyList().init shouldBe MyList()
		MyList(1).init shouldBe MyList()
		MyList(1, 2, 3).init shouldBe MyList(1, 2)
	}

	"foldLeft" should "collapse left to right" in {
		MyList("a", "b", "c").foldLeft("d"){_ + _} shouldBe "dabc"
		MyList.empty[String].foldRight("d"){_ + _} shouldBe "d"
	}

	"foldRight" should "collapse right to left" in {
		MyList("a", "b", "c").foldRight("d"){_ + _} shouldBe "abcd"
		MyList.empty[String].foldRight("d"){_ + _} shouldBe "d"
	}

	"reverse" should "return elements in a reverse order" in {
		MyList(1, 2, 3).reverse shouldBe MyList(3, 2, 1)
		MyList(1).reverse shouldBe MyList(1)
		MyList().reverse shouldBe MyList()
	}

	"append" should "append 2 lists" in {
		MyList() append MyList() shouldBe MyList()
		MyList(1, 2, 3) append MyList() shouldBe MyList(1, 2, 3)
		MyList() append MyList(1, 2, 3) shouldBe MyList(1, 2, 3)
		MyList(1, 2, 3) append MyList(4, 5, 6) shouldBe MyList(1, 2, 3, 4, 5, 6)
	}

	"concat" should "concatenate lists" in {
		MyList.concat(MyList()) shouldBe MyList()
		MyList.concat(MyList(MyList())) shouldBe MyList()
		MyList.concat(MyList(MyList(), MyList())) shouldBe MyList()
		MyList.concat(MyList(MyList(1, 2))) shouldBe MyList(1, 2)
		MyList.concat(MyList(MyList(1, 2), MyList())) shouldBe MyList(1, 2)
		MyList.concat(MyList(MyList(), MyList(1, 2))) shouldBe MyList(1, 2)
		MyList.concat(MyList(MyList(1, 2), MyList(3, 4))) shouldBe MyList(1, 2, 3, 4)
	}

	"map" should "transform each element" in {
		MyList(1, 2, 3) map {_ * 2} shouldBe MyList(2, 4, 6)
		MyList.empty[Int] map {_ * 2} shouldBe MyList()
	}

	"flatMap" should "map each element onto a least and flatten the result" in {
		MyList(1, 3, 5) flatMap {x => MyList(x, x + 1)} shouldBe MyList(1, 2, 3, 4, 5, 6)
		MyList.empty[Int] flatMap {x => MyList(x, x + 1)} shouldBe MyList()
	}

	"filter" should "remove all elements unless they satisfy a given predicate" in {
		MyList(1, 2, 3) filter {_ % 2 != 0} shouldBe MyList(1, 3)
		MyList(2, 4) filter {_ % 2 != 0} shouldBe MyList()
		MyList.empty[Int] filter {_ % 2 != 0} shouldBe MyList()
	}

	"zip" should "transform each pair with the same index" in {
		MyList.zip(MyList(1, 2, 3), MyList(10, 20, 30))(_ + _, identity, identity) shouldBe MyList(11, 22, 33)
		MyList.zip(MyList(1, 2, 3), MyList(10, 20))(_ + _, identity, identity) shouldBe MyList(11, 22, 3)
		MyList.zip(MyList(1, 2), MyList(10, 20, 30))(_ + _, identity, identity) shouldBe MyList(11, 22, 30)
		MyList.zip(MyList(1, 2, 3), MyList.empty[Int])(_ + _, identity, identity) shouldBe MyList(1, 2, 3)
		MyList.zip(MyList.empty[Int], MyList(1, 2, 3))(_ + _, identity, identity) shouldBe MyList(1, 2, 3)
		MyList.zip(MyList.empty[Int], MyList.empty[Int])(_ + _, identity, identity) shouldBe MyList()
	}

	"hasSubsequence" should "return true iff the first list contains the second one a a subsequence" in {
		hasSubsequence(MyList(1, 2, 3, 4), MyList(2, 3)) shouldBe true
		hasSubsequence(MyList(1, 2, 3, 4), MyList(1, 3)) shouldBe false
		hasSubsequence(MyList(1, 2, 3, 4), MyList(3, 2)) shouldBe false
		hasSubsequence(MyList(1, 2, 3, 4), MyList(1, 2, 3, 4)) shouldBe true
		hasSubsequence(MyList(2, 3), MyList(1, 2, 3, 4)) shouldBe false
		hasSubsequence(MyList(2, 3), MyList(2, 3, 4)) shouldBe false
		hasSubsequence(MyList(), MyList()) shouldBe true
	}
}