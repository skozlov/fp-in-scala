package com.github.skozlov.fpinscala

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

	"foldRight" should "collapse right to left" in {
		MyList("a", "b", "c").foldRight("d"){_ + _} shouldBe "abcd"
		MyList.empty[String].foldRight("d"){_ + _} shouldBe "d"
	}
}