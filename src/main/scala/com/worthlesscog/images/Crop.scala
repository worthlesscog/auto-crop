package com.worthlesscog.images

import com.worthlesscog.images.CropOperation._
import com.worthlesscog.images.Edge._
import org.opencv.core.{Core, Mat, MatOfInt, MatOfPoint, Point, Rect, Size}
import org.opencv.imgcodecs.Imgcodecs
import org.opencv.imgproc.Imgproc

import java.io.File
import java.util.ArrayList
import scala.math.{abs, atan, toDegrees}

object Crop:

    implicit class RichMat(m: Mat):
        def apply(r: Int, c: Int) =
            m.get(r, c)(0)

    val CANNY_SIZE = 1024
    val WIP_QUALITY = 75

    def crop(p: CropParameters): Unit =
        load(p.sourceImage) match
            case None    => println(s"Can't load ${p.sourceImage}")
            case Some(i) => p.op match
                case CannyOnly => cannyOnly(i, p)
                case SquareUp  => cannyThenSquare(i, p)

    private def load(image: String) =
        val i = Imgcodecs.imread(image)
        if (i empty)
            None
        else
            Some(i)

    def cannyOnly(image: Mat, p: CropParameters) =
        grayToCanny(p, true, 0)(image)

    private def grayToCanny(p: CropParameters, showSteps: Boolean, from: Int) =
        grayscale andThen
        save(p.sourceImage, s"_${from}-grayscale.jpg", p.showSteps) andThen
        gaussianBlur(5, 1.0, 1.0) andThen
        save(p.sourceImage, s"_${from + 1}-blur.jpg", p.showSteps) andThen
        resize(CANNY_SIZE) andThen
        save(p.sourceImage, s"_${from + 2}-resize.jpg", p.showSteps) andThen
        autoCanny(p.sigma) andThen
        // canny(50.0, 200.0, 3) andThen
        save(p.sourceImage, s"_${from + 3}-autocanny.jpg", showSteps)

    def grayscale(i: Mat) =
        val o = Mat()
        Imgproc.cvtColor(i, o, Imgproc.COLOR_RGB2GRAY)
        o

    private def save(source: String, image: String, actuallySave: Boolean = false)(i: Mat): Mat =
        if (actuallySave)
            save(source, image, WIP_QUALITY, true)(i)
        else
            i

    def save(source: String, image: String, quality: Int, overwrite: Boolean)(i: Mat) =
        val f = File(File(source).getParent, image)
        val o = if (f.exists && !overwrite) next(f) else f
        val flags = MatOfInt(Imgcodecs.IMWRITE_JPEG_QUALITY, quality)
        Imgcodecs.imwrite(o.toString, i, flags)
        i

    private def next(f: File) =
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

    def gaussianBlur(kernelSize: Int, sigmaX: Double, sigmaY: Double)(i: Mat) =
        val o = Mat()
        Imgproc.GaussianBlur(i, o, Size(kernelSize, kernelSize), sigmaX, sigmaY)
        o

    def resize(width: Int)(i: Mat) =
        val f = width.toDouble / i.cols
        val o = Mat()
        Imgproc.resize(i, o, Size(), f, f, Imgproc.INTER_AREA)
        o

    // https://www.pyimagesearch.com/2015/04/06/zero-parameter-automatic-canny-edge-detection-with-python-and-opencv/
    // A lower value of sigma indicates a tighter threshold, whereas
    // a larger value of sigma gives a wider threshold.
    def autoCanny(sigma: Double)(i: Mat) =
        println(s"sigma $sigma")
        val v = median(i)
        val l = (0.0 max (1.0 - sigma) * v).toInt
        val u = (255.0 min (1.0 + sigma) * v).toInt
        val o = Mat()
        Imgproc.Canny(i, o, l, u)
        o

    // XXX - assuming what numpy.median actually does
    private def median(i: Mat): Double =
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

    private def median(a: Array[Short]): Double =
        val l = a.length
        if (odd(l))
            a((l - 1) / 2)
        else
            (a((l - 1) / 2) + a(l / 2)) / 2.0

    def odd(i: Int) =
        i % 2 != 0

    def cannyThenSquare(image: Mat, p: CropParameters) =
        val c1 = grayToCanny(p, p.showSteps, 0)(image)
        val r = rotateSquare(image, p)(c1)
        val c2 = grayToCanny(p, p.showSteps, 5)(r)
        finalCrop(r, p)(c2)

    private def rotateSquare(image: Mat, p: CropParameters) =
        rotate(image, p.alignmentEdge, p.angleAdjustment) andThen
        save(p.sourceImage, "_4-rotate.jpg", p.showSteps)

    // def canny(hysteresisThreshold1: Double, hysteresisThreshold2: Double, sobelAperture: Int)(i: Mat) =
    //     val o = Mat()
    //     Imgproc.Canny(i, o, hysteresisThreshold1, hysteresisThreshold2, sobelAperture, true)
    //     o

    // see https://www.pyimagesearch.com/2017/01/02/rotate-images-correctly-with-opencv-and-python/
    private def rotate(original: Mat, edge: Edge, angularAdjustment: Double)(i: Mat) =
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
        o

    private def findEdge(edge: Edge, i: Mat): (Vector, Point) =
        edge match
            case Top       => findEdge(i, topDown, true)
            case RightSide => findEdge(i, rightToLeft, false)
            case Bottom    => findEdge(i, bottomUp, true)
            case LeftSide  => findEdge(i, leftToRight, false)

    // XXX - return should be Optional, what if you can't find enough points to make a line?
    private def findEdge(i: Mat, f: (Mat, Int) => Option[Int], horizontal: Boolean) =
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

    private def topDown(i: Mat, c: Int) =
        verticalScan(0 until i.rows / 4, i, c)

    private def verticalScan(rows: Range, i: Mat, c: Int) =
        rows find { i.get(_, c).sum != 0.0 }

    private def rightToLeft(i: Mat, r: Int) =
        horizontalScan(i.cols - 1 to i.cols - i.cols / 4 by -1, i, r)

    private def horizontalScan(cols: Range, i: Mat, r: Int) =
        cols find { i.get(r, _).sum != 0.0 }

    private def bottomUp(i: Mat, c: Int) =
        verticalScan(i.rows - 1 to i.rows - i.rows / 4 by -1, i, c)

    private def leftToRight(i: Mat, r: Int) =
        horizontalScan(0 until i.cols / 4, i, r)

    private def fitLine(points: ArrayList[Point]) =
        val m = MatOfPoint()
        m.fromList(points)
        val o = Mat()
        Imgproc.fitLine(m, o, Imgproc.DIST_L2, 0.0, 0.01, 0.01)
        // In case of 2D fitting, it should be a vector of 4 elements -
        // (vx, vy, x0, y0), where (vx, vy) is a normalized vector collinear
        // to the line and (x0, y0) is a point on the line.
        (Vector(o(0, 0), o(1, 0)), Point(o(2, 0), o(3, 0)))

    private def finalCrop(image: Mat, p: CropParameters) =
        crop(image, p.margins) andThen
        save(p.sourceImage, p.targetImage, p.saveQuality, p.overwrite)

    // XXX - should check lines and only crop lines that are correctly oriented
    private def crop(original: Mat, m: Margins)(i: Mat) =
        val (_, t) = findEdge(Top, i)
        val (_, b) = findEdge(Bottom, i)
        val (_, l) = findEdge(LeftSide, i)
        val (_, r) = findEdge(RightSide, i)

        val (sx, sy) = (original.cols / i.cols.toDouble, original.rows / i.rows.toDouble)

        val crop = Rect(Point(l.x * sx + m.l, t.y * sy + m.t), Point(r.x * sx - m.r, b.y * sy - m.b))
        original.submat(crop)
