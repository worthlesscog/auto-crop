package com.worthlesscog.images

import com.worthlesscog.images.Edge._
import org.opencv.core.{Core, Mat, MatOfInt, MatOfPoint, Point, Rect, Size}
import org.opencv.imgcodecs.Imgcodecs
import org.opencv.imgproc.Imgproc

import java.io.File
import java.util.ArrayList
import scala.math.{abs, atan, toDegrees}

object AutoCrop:

    implicit class RichMat(m: Mat):
        def apply(r: Int, c: Int) =
            m.get(r, c)(0)

    val CANNY_SIZE = 1024
    val WIP_QUALITY = 75

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
            case Right(c)   => scan(c)

    def parseControls(args: List[String], c: ScanControls): Either[String, ScanControls] =
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

    def scan(c: ScanControls) =
        load(c.sourceImage) match
            case Some(i) => c.op(c, i)
            case None    => println(s"Can't load ${c.sourceImage}")

    def cannyOnly(c: ScanControls, image: Mat) =
        grayToCanny(c, true)(image)

    def cannyThenSquare(c: ScanControls, image: Mat) =
        (grayToCanny(c, c.showSteps) andThen autoCrop(c)) (image)

    def grayToCanny(c: ScanControls, showSteps: Boolean) =
        grayscale andThen
        save(c.sourceImage, "_0.jpg", c.showSteps) andThen
        gaussianBlur(5, 1.0, 1.0) andThen
        resize(CANNY_SIZE) andThen
        autoCanny(c.sigma) andThen
        // canny(50.0, 200.0, 3) andThen
        save(c.sourceImage, "_1.jpg", showSteps)

    def autoCrop(c: ScanControls) =
        rotate(c.alignmentEdge, c.angleAdjustment) andThen
        save(c.sourceImage, "_2.jpg", c.showSteps) andThen
        grayscale2 andThen
        gaussianBlur(5, 1.0, 1.0) andThen
        resize(CANNY_SIZE) andThen
        autoCanny(c.sigma) andThen
        // canny(50.0, 200.0, 3) andThen
        save(c.sourceImage, "_3.jpg", c.showSteps) andThen
        crop(c.margins) andThen
        save(c.sourceImage, c.targetImage, c.saveQuality, c.overwrite)

    def load(image: String) =
        val i = Imgcodecs.imread(image)
        if (i empty)
            None
        else
            Some(i)

    def grayscale(i: Mat) =
        val o = Mat()
        Imgproc.cvtColor(i, o, Imgproc.COLOR_RGB2GRAY)
        (i, o)

    def gaussianBlur(kernelSize: Int, sigmaX: Double, sigmaY: Double)(original: Mat, i: Mat) =
        val o = Mat()
        Imgproc.GaussianBlur(i, o, Size(kernelSize, kernelSize), sigmaX, sigmaY)
        (original, o)

    def resize(width: Int)(original: Mat, i: Mat) =
        val f = width.toDouble / i.cols
        val o = Mat()
        Imgproc.resize(i, o, Size(), f, f, Imgproc.INTER_AREA)
        (original, o)

    // https://www.pyimagesearch.com/2015/04/06/zero-parameter-automatic-canny-edge-detection-with-python-and-opencv/
    // A lower value of sigma indicates a tighter threshold, whereas
    // a larger value of sigma gives a wider threshold.
    def autoCanny(sigma: Double)(original: Mat, i: Mat) =
        val v = median(i)
        val l = (0.0 max (1.0 - sigma) * v).toInt
        val u = (255.0 min (1.0 + sigma) * v).toInt
        val o = Mat()
        Imgproc.Canny(i, o, l, u)
        (original, o)

    // XXX - assuming what numpy.median actually does
    def median(i: Mat): Double =
        val p = new Array[Byte](i.channels)
        val values = i.cols * i.rows * i.channels
        // XXX - 2x wastage, these should be bytes but bytes are signed, stupid java
        val shorts = new Array[Short](values)
        var idx = 0
        for (x <- 0 until i.cols)
            for (y <- 0 until i.rows)
                i.get(y, x, p)
                for (c <- 0 until i.channels)
                    shorts(idx + c) = (p(c) & 0xff).toShort
                idx += i.channels
        // XXX - slow
        util.Sorting.quickSort(shorts)
        median(shorts)

    def median(a: Array[Short]): Double =
        val l = a.length
        if (odd(l))
            a((l - 1) / 2)
        else
            (a((l - 1) / 2) + a(l / 2)) / 2.0

    def odd(i: Int) =
        i % 2 != 0

    // def canny(hysteresisThreshold1: Double, hysteresisThreshold2: Double, sobelAperture: Int)(original: Mat, i: Mat) =
    //     val o = Mat()
    //     Imgproc.Canny(i, o, hysteresisThreshold1, hysteresisThreshold2, sobelAperture, true)
    //     (original, o)

    def save(source: String, image: String, actuallySave: Boolean = false)(original: Mat, i: Mat): (Mat, Mat) =
        if (actuallySave)
            save(source, image, WIP_QUALITY, true)(original, i)
        else
            (original, i)

    def save(source: String, image: String, quality: Int, overwrite: Boolean)(original: Mat, i: Mat) =
        val f = File(File(source).getParent, image)
        val o = if (f.exists && !overwrite) next(f) else f
        val flags = MatOfInt(Imgcodecs.IMWRITE_JPEG_QUALITY, quality)
        Imgcodecs.imwrite(o.toString, i, flags)
        (original, i)

    def next(f: File) =
        val (p, n, s) = split(f)
        var i = 2
        var o: File = null
        while
            o = new File(p, s"$n$i.$s")
            o.exists
        do
            i += 1
        o

    def split(f: File) =
        val n = f.getName
        n.lastIndexOf('.') match
            case -1 => (f.getParent, n, "")
            case i  => (f.getParent, n.substring(0, i), n.substring(i + 1))

    // see https://www.pyimagesearch.com/2017/01/02/rotate-images-correctly-with-opencv-and-python/
    def rotate(edge: Edge, angularAdjustment: Double)(originalAndCannied: (Mat, Mat)) =
        val (original, i) = (originalAndCannied._1, originalAndCannied._2)
        val (v, _) = findEdge(edge, i)
        // XXX - will fail if line is vertical, vx will be 0
        val angle = edge match
            case Bottom | Top         => toDegrees(atan(v.vy / v.vx)) - angularAdjustment
            case LeftSide | RightSide => 90.0 - toDegrees(atan(v.vy / v.vx)) - angularAdjustment
        println(s"skew $angle°")
        val m = Imgproc.getRotationMatrix2D(Point(0, 0), angle, 1.0)
        val (cos, sin) = (abs(m(0, 0)), abs(m(0, 1)))
        val (w, h) = (original.cols, original.rows)
        val nW = (h * sin + w * cos).toInt
        val nH = (h * cos + w * sin).toInt
        m.put(0, 2, m(0, 2) + nW / 2.0 - w / 2)
        m.put(1, 2, m(1, 2) + nH / 2.0 - h / 2)
        val o = new Mat()
        Imgproc.warpAffine(original, o, m, Size(nW, nH), Imgproc.INTER_AREA, Core.BORDER_REFLECT_101)
        (original, o)

    def findEdge(edge: Edge, i: Mat): (Vector, Point) =
        edge match
            case Top       => findEdge(i, topDown, true)
            case RightSide => findEdge(i, rightToLeft, false)
            case Bottom    => findEdge(i, bottomUp, true)
            case LeftSide  => findEdge(i, leftToRight, false)

    // XXX - return should be Optional, what if you can't find enough points to make a line?
    def findEdge(i: Mat, f: (Mat, Int) => Option[Int], horizontal: Boolean) =
        val points = ArrayList[Point]

        if (horizontal)
            val w = i.cols / 5
            for (c <- w until i.cols - w)
                f(i, c) map { points add Point(c, _) }
        else
            val w = i.rows / 5
            for (r <- w until i.rows - w)
                f(i, r) map { points add Point(_, r) }

        fitLine(points)

    def topDown(i: Mat, c: Int) =
        verticalScan(0 until i.rows / 4, i, c)

    def bottomUp(i: Mat, c: Int) =
        verticalScan(i.rows - 1 to i.rows - i.rows / 4 by -1, i, c)

    def verticalScan(rows: Range, i: Mat, c: Int) =
        rows find { i.get(_, c).sum != 0.0 }

    def leftToRight(i: Mat, r: Int) =
        horizontalScan(0 until i.cols / 4, i, r)

    def rightToLeft(i: Mat, r: Int) =
        horizontalScan(i.cols - 1 to i.cols - i.cols / 4 by -1, i, r)

    def horizontalScan(cols: Range, i: Mat, r: Int) =
        cols find { i.get(r, _).sum != 0.0 }

    def fitLine(points: ArrayList[Point]) =
        val m = MatOfPoint()
        m.fromList(points)
        val o = Mat()
        Imgproc.fitLine(m, o, Imgproc.DIST_L2, 0.0, 0.01, 0.01)
        // In case of 2D fitting, it should be a vector of 4 elements -
        // (vx, vy, x0, y0), where (vx, vy) is a normalized vector collinear
        // to the line and (x0, y0) is a point on the line.
        (Vector(o(0, 0), o(1, 0)), Point(o(2, 0), o(3, 0)))

    def grayscale2(original: Mat, i: Mat) =
        grayscale(i)

    // XXX - should check lines and only crop lines that are correctly oriented
    def crop(m: Margins)(original: Mat, i: Mat) =
        val (_, t) = findEdge(Top, i)
        val (_, b) = findEdge(Bottom, i)
        val (_, l) = findEdge(LeftSide, i)
        val (_, r) = findEdge(RightSide, i)

        val (sx, sy) = (original.cols / i.cols.toDouble, original.rows / i.rows.toDouble)

        val crop = Rect(Point(l.x * sx + m.l, t.y * sy + m.t), Point(r.x * sx - m.r, b.y * sy - m.b))
        (original, original.submat(crop))
