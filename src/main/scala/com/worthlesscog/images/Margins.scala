package com.worthlesscog.images

case class Margins(t: Int, l: Int, b: Int, r: Int)

object Margins:

    def apply(all: String): Margins = new Margins(all toInt, all toInt, all toInt, all toInt)

    def apply(t: String, l: String): Margins = new Margins(t toInt, l toInt, 0, 0)
    def apply(t: String, l: String, b: String): Margins = new Margins(t toInt, l toInt, b toInt, 0)
    def apply(t: String, l: String, b: String, r: String): Margins = new Margins(t toInt, l toInt, b toInt, r toInt)
