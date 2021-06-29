package com.worthlesscog.images

import com.worthlesscog.images.Edge._
import org.opencv.core.Mat

case class ScanControls(
    alignmentEdge: Edge = Top,
    angleAdjustment: Double = 0.0,
    margins: Margins = Margins(0, 0, 0, 0),
    op: (Mat, ScanControls) => Unit,
    overwrite: Boolean = false,
    saveQuality: Int = 100,
    showSteps: Boolean = false,
    sigma: Double = 0.33,
    sourceImage: String = "",
    targetImage: String = "output.jpg"
)
