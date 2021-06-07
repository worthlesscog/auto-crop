package com.worthlesscog.images

case class Margins(t: Int, b: Int, l: Int, r: Int)

object Margins:

    def apply(all: String): Margins = new Margins(all toInt, all toInt, all toInt, all toInt)

    def apply(t: String, b: String, l: String, r: String): Margins = new Margins(t toInt, b toInt, l toInt, r toInt)
