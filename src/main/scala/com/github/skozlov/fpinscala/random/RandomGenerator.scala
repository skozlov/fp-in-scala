package com.github.skozlov.fpinscala.random

trait RandomGenerator {
	def nextInt: (Int, RandomGenerator)
}