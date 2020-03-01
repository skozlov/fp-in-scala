package com.github.skozlov.fpinscala

import com.github.skozlov.fpinscala.state.State

package object random {
	type Random[+A] = State[RandomGenerator, A]
}