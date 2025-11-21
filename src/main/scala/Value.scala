package edu.luc.cs.laufer.cs371.expressions

sealed trait Value
case class Num(value: Int) extends Value
