package com.worthlesscog.images

import com.worthlesscog.images.Edge._
import org.opencv.core.Mat

case class ScanControls(
    alignmentEdge: Edge = Top,
    angleAdjustment: Double = 0.0,
    margins: Margins = Margins(0, 0, 0, 0),
    op: (ScanControls, Mat) => Unit,
    outputImage: String = "output.jpg",
    saveQuality: Int = 100,
    showSteps: Boolean = false,
    sigma: Double = 0.33,
    sourceImage: String = ""
)
