package com.worthlesscog.images

import com.worthlesscog.images.Edge._
import com.worthlesscog.images.Operation._

case class ScanControls(
    alignmentEdge: Edge = Top,
    angleAdjustment: Double = 0.0,
    margins: Margins = Margins(0, 0, 0, 0),
    operation: Operation = Square,
    outputImage: String = "output.jpg",
    saveQuality: Int = 100,
    showSteps: Boolean = false,
    sigma: Double = 0.33,
    sourceImage: String = ""
)
