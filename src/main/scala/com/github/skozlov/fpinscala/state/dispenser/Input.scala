package com.github.skozlov.fpinscala.state.dispenser

sealed trait Input

object Input {
	case object Coin extends Input
	case object Turn extends Input
}