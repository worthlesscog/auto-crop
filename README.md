## Scan Auto-crop
Work in Progress.

Workflow tool for auto-aligning and cropping scanned images.

Scanners don't always scan the edges of targets especially
at the edge of the glass. Scanning targets floating in the 
middle of the glass inevitably means they're not square and
have to be manually aligned and cropped in an image editor.
That sucks.

This tool uses OpenCV to find the floating target in a scan
to automatically square it up and crop it.

Used currently for cropping CD artwork at high resolution. Written
in Scala 3 to play with some of the new toys. Ooh! Whitespace!
Working fine in JDK 11.0.9 on Windows 10.
### Build
```
sbt assembly
```
The resulting executable jar is large as it contains the native
binaries packaged by OpenPnP. Get thee to a script file to launch
it and pass parameters.
### Usage
```
autocrop filename switches
```
|Switch|Description|
|---|---|
|-a 0.13|Angle tweak for when artwork isn't properly aligned with crop edge, typically small i.e -0.1|
|-c|Stop after Canny edge detection and dump the Canny image|
|-(blr)|Specify bottom, left or right as alignment edge, defaults to top|
|-i|Dump intermediate work images, off by default|
|-m 25|Set ragged edge crop margin, defaults to 0, the detected edge line|
|-m 5,5,10,0|Set ragged edge crop margins for bottom, left, right and top respectively|
|-o file|Output image name, defaults to "output.jpg"|
|-q 95|JPEG save quality, defaults to 100|
|-sigma 0.45|Canny threshold control, defaults to 0.33|
### Limitations
* It's a bit rough and ready. It works though. "Mostly"
* JPEG's only
### TODO
* Don't try to align images if they're already aligned.
* Only crop edges that you can find a good line for.
* Work out what the Canny parameters actually do.
* Work out what the Gaussian blur parameters actually do.
* Work out if Hough can be used instead of my hokey-cokey line algorithm.
* Find a better median algorithm.
### References
* https://opencv.org/
* https://github.com/openpnp/opencv
* https://www.pyimagesearch.com/2015/04/06/zero-parameter-automatic-canny-edge-detection-with-python-and-opencv/
* https://www.pyimagesearch.com/2017/01/02/rotate-images-correctly-with-opencv-and-python/
