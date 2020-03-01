package com.github.skozlov.fpinscala.state

case class State[S, +A](run: S => (A, S)){
	import State._

	def flatMap[B](f: A => State[S, B]): State[S, B] = State {initState => {
		val (a, afterA) = run(initState)
		val stateB = f(a)
		stateB.run(afterA)
	}}

	def map[B](f: A => B): State[S, B] = flatMap {f andThen const}

	def map2[B, C](that: State[S, B])(f: (A, B) => C): State[S, C] = {
		for {
			a <- this
			b <- that
		} yield f(a, b)
	}
}

object State {
	def const[S, A](a: A): State[S, A] = State {s => (a, s)}

	def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] = {
		val reversed = states.foldLeft[State[S, List[A]]](const(Nil)){
			(prefixReversed, state) => prefixReversed.map2(state){
				(prefixReversed, a) => a :: prefixReversed
			}
		}
		reversed map {_.reverse}
	}

	def get[S]: State[S, S] = State(s => (s, s))

	def set[S](s: S): State[S, Unit] = State(_ => ((), s))

	def modify[S](f: S => S): State[S, Unit] = {
		for {
			s <- get
			_ <- set(f(s))
		} yield ()
	}

	def modify[S](fs: List[S => S]): State[S, Unit] = modify {s => fs.foldLeft(s){(s, f) => f(s)} }
}