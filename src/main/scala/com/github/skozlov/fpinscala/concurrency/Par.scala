package com.github.skozlov.fpinscala.concurrency

trait Par[+A]

object Par{
	def unit[A](a: => A): Par[A] = ???

	def fork[A](a: => Par[A]): Par[A] = ???

	def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???
}