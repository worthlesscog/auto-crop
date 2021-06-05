package com.worthlesscog.images

case class Margins(b: Int, l: Int, r: Int, t: Int)

object Margins:

    def apply(all: String): Margins = new Margins(all toInt, all toInt, all toInt, all toInt)

    def apply(b: String, l: String, r: String, t: String): Margins = new Margins(b toInt, l toInt, r toInt, t toInt)
