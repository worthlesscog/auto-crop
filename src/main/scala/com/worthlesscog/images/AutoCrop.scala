package com.worthlesscog.images

import com.worthlesscog.images.CropOperation._
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

        parseControls(args toList, CropParameters()) match
            case Left(oops) => println(oops)
            case Right(c)   => Crop.crop(c)

    private def parseControls(args: List[String], p: CropParameters): Either[String, CropParameters] =
        if (args isEmpty)
            if (p.source isEmpty)
                Left("Image?")
            else
                Right(p)
        else args match
            case "-a" :: FLOAT(a) :: t              => parseControls(t, p.copy(angleAdjustment = a.toDouble % 360.0))
            case ("-b" | "-l" | "-r") :: t          => parseControls(t, p.copy(alignmentEdge = edges(args head)))
            case "-c" :: t                          => parseControls(t, p.copy(op = CannyOnly))
            case "-i" :: t                          => parseControls(t, p.copy(showSteps = true))
            case "-m" :: MARGIN(m) :: t             => parseControls(t, p.copy(margins = Margins(m)))
            case "-m" :: MARGIN2(top, l) :: t       => parseControls(t, p.copy(margins = Margins(top, l)))
            case "-m" :: MARGIN3(top, l, b) :: t    => parseControls(t, p.copy(margins = Margins(top, l, b)))
            case "-m" :: MARGIN4(top, l, b, r) :: t => parseControls(t, p.copy(margins = Margins(top, l, b, r)))
            case "-o" :: t                          => parseControls(t, p.copy(overwrite = true))
            case "-q" :: POS_INT(q) :: t            => parseControls(t, p.copy(quality = 100 min q.toInt))
            case "-sigma" :: FLOAT(s) :: t          => parseControls(t, p.copy(sigma = s.toDouble))
            case "-t" :: o :: t                     => parseControls(t, p.copy(target = o))
            case path :: t                          => parseControls(t, p.copy(source = path))
            case _                                  => parseControls(Nil, p)
