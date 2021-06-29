package com.worthlesscog.images

import com.worthlesscog.images.Crop.{cannyOnly, cannyThenSquare, crop}
import com.worthlesscog.images.Edge._
import org.opencv.core.Core

import java.io.File
import java.util.ArrayList
import scala.math.{abs, atan, toDegrees}

object AutoCrop:

    val DIGITS = "\\d+"
    val TRIPLET = "\\d{1,3}"
    val INT = "-?" + TRIPLET
    val FLT = INT + "\\." + DIGITS

    val CAP_FLT = s"($FLT)"
    val CAP_INT = s"($INT)"
    val CAP_TRIPLET = s"($TRIPLET)"

    val POS_INT = s"^$CAP_TRIPLET$$".r
    val FLOAT = s"^$CAP_FLT$$".r
    val MARGIN = s"^$CAP_INT$$".r
    val MARGIN2 = s"^$CAP_INT,$CAP_INT$$".r
    val MARGIN3 = s"^$CAP_INT,$CAP_INT,$CAP_INT$$".r
    val MARGIN4 = s"^$CAP_INT,$CAP_INT,$CAP_INT,$CAP_INT$$".r

    val edges = Map("-b" -> Bottom, "-l" -> LeftSide, "-r" -> RightSide)

    def main(args: Array[String]): Unit =

        nu.pattern.OpenCV.loadShared()
        System.loadLibrary(Core.NATIVE_LIBRARY_NAME)

        parseControls(args toList, ScanControls(op = cannyThenSquare)) match
            case Left(oops) => println(oops)
            case Right(c)   => crop(c)

    private def parseControls(args: List[String], c: ScanControls): Either[String, ScanControls] =
        if (args isEmpty)
            if (c.sourceImage isEmpty)
                Left("Image?")
            else
                Right(c)
        else args match
            case "-a" :: FLOAT(a) :: t              => parseControls(t, c.copy(angleAdjustment = a.toDouble % 360.0))
            case ("-b" | "-l" | "-r") :: t          => parseControls(t, c.copy(alignmentEdge = edges(args head)))
            case "-c" :: t                          => parseControls(t, c.copy(op = cannyOnly))
            case "-i" :: t                          => parseControls(t, c.copy(showSteps = true))
            case "-m" :: MARGIN(m) :: t             => parseControls(t, c.copy(margins = Margins(m)))
            case "-m" :: MARGIN2(top, l) :: t       => parseControls(t, c.copy(margins = Margins(top, l)))
            case "-m" :: MARGIN3(top, l, b) :: t    => parseControls(t, c.copy(margins = Margins(top, l, b)))
            case "-m" :: MARGIN4(top, l, b, r) :: t => parseControls(t, c.copy(margins = Margins(top, l, b, r)))
            case "-o" :: t                          => parseControls(t, c.copy(overwrite = true))
            case "-q" :: POS_INT(q) :: t            => parseControls(t, c.copy(saveQuality = 100 min q.toInt))
            case "-sigma" :: FLOAT(s) :: t          => parseControls(t, c.copy(sigma = s.toDouble))
            case "-t" :: o :: t                     => parseControls(t, c.copy(targetImage = o))
            case path :: t                          => parseControls(t, c.copy(sourceImage = path))
            case _                                  => parseControls(Nil, c)
