package com.github.skozlov.fpinscala.random

case class SimpleRandomGenerator(seed: Long) extends RandomGenerator {
	override def nextInt: (Int, RandomGenerator) = {
		val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
		((newSeed >>> 16).toInt, SimpleRandomGenerator(newSeed))
	}
}