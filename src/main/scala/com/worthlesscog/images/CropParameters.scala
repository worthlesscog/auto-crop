package com.worthlesscog.images

import com.worthlesscog.images.CropOperation._
import com.worthlesscog.images.Edge._
import org.opencv.core.Mat

case class CropParameters(
    alignmentEdge: Edge = Top,
    angleAdjustment: Double = 0.0,
    margins: Margins = Margins(0, 0, 0, 0),
    op: CropOperation = SquareUp,
    overwrite: Boolean = false,
    quality: Int = 100,
    showSteps: Boolean = false,
    sigma: Double = 0.33,
    source: String = "",
    target: String = "output.jpg"
)
