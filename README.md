<!-- README.md is generated from README.Rmd. Please edit that file -->

# mapscanner

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/mpadge/mapscanner.svg?branch=master)](https://travis-ci.org/mpadge/mapscanner)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/mpadge/mapscanner?branch=master&svg=true)](https://ci.appveyor.com/project/mpadge/mapscanner)
<!-- badges: end -->

Print maps, draw on them, scan them back in, and convert to spatial
objects. Package comes with a sample map of Omaha, Nebraska, USA, and
one with some red lines drawn on it: ![](./inst/extdata/omaha_drawn.jpg)

That’s just a standard `jpeg` image with no notion of geographical
coordinates. The original map was generated with

``` r
devtools::load_all (".", export_all = FALSE)
#> Loading mapscanner
```

``` r
bbox <- rbind (c (-96.12923, -96.01011),
               c (41.26145, 41.32220))
omaha <- ms_get_map (loc, max_tiles = 16L)
```

That map it itself a `RasterBrick` object from the [`raster`
package](https://cran.r-project.org/package=raster), which can be
converted to `pdf` format for printing with

``` r
ms_map_to_pdf (omaha, file = "omaha")
```

That version can then be printed out and either scanned back in, or
simply photographed with a mobile device. The original `raster` object
is all that’s need to rectify the scanned image with the original map.
The magic is performed via the [`RNiftyReg`
package](https://github.com/jonclayden/RNiftyReg), itself primarily
intended to align brain scans and other medical images, but which is
precisely the tool needed here.

To try it out here, we can use the [`magick`
package](https://github.com/ropensci/magick) to convert the internally
bundled image files to `pdf` versions:

``` r
system.file ("extdata", "omaha.jpg", package = "mapscanner") %>%
    magick::image_read () %>%
    magick::image_write (path = "omaha.pdf", format = "pdf")
system.file ("extdata", "omaha_drawn.jpg", package = "mapscanner") %>%
    magick::image_read () %>%
    magick::image_write (path = "omaha.pdf", format = "pdf")
```

That gives us `pdf` versions of the above image file, and the original
before it was drawn on. The two can be rectified with the single
command:

``` r
result <- scan_maps ("omaha.pdf", "omaha_drawn.pdf")
```
