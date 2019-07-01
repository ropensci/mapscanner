<!-- README.md is generated from README.Rmd. Please edit that file -->

# mapscanner

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/mpadge/mapscanner.svg?branch=master)](https://travis-ci.org/mpadge/mapscanner)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/mpadge/mapscanner?branch=master&svg=true)](https://ci.appveyor.com/project/mpadge/mapscanner)
[![codecov](https://codecov.io/gh/mpadge/mapscanner/branch/master/graph/badge.svg)](https://codecov.io/gh/mpadge/mapscanner)
[![Project Status:
Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

Print maps, draw on them, scan them back in, and convert to spatial
objects. Package comes with a sample map of Omaha, Nebraska, USA, and
one with some red lines drawn on it: ![](./inst/extdata/omaha_drawn.jpg)

That’s just a standard `jpeg` image with no notion of geographical
coordinates. The original map was generated with

``` r
bbox <- osmdata:getbb ("omaha nebraska")
ms_generate_map (bbox, max_tiles = 16L, mapname = "omaha")
```

    #> Successfully generated 'omaha.pdf' and 'omaha.jpg'

As indicated, the function generates a map in both `.pdf` and `.jpg`
formats. These files must be retained as the “master” maps against which
subsequently modified – draw-over and scanned-in – versions will be
rectified. The `.pdf` format is generated because it will generally be
the most convenient for printing, while the rectification itself
requires `.jpg`-format images. The magic happens via the [`RNiftyReg`
package](https://github.com/jonclayden/RNiftyReg), itself primarily
intended to align brain scans and other medical images, but which is
precisely the tool needed here.

The `mapscanner` package comes with two sample `.jpg` images which can
be used to demonstrate functionality. In the following code,
`f_modified` is the image shown above, modified from the original by
drawing a red line around a particular region of Omaha.

``` r
f_original <- file.path ("inst", "extdata", "omaha.jpg")
f_modified <- file.path ("inst", "extdata", "omaha_drawn.jpg")
system.time (res <- ms_rectify_maps (f_original, f_modified, type = "polygons"))
#>    user  system elapsed 
#>  43.713   0.750  12.455
res
#> Simple feature collection with 2 features and 0 fields
#> geometry type:  POLYGON
#> dimension:      XY
#> bbox:           xmin: -96.11759 ymin: 41.26661 xmax: -96.02757 ymax: 41.30084
#> epsg (SRID):    4326
#> proj4string:    +proj=longlat +datum=WGS84 +no_defs
#>                         geometry
#> 1 POLYGON ((-96.10715 41.2685...
#> 2 POLYGON ((-96.02782 41.2962...
```

The rectification can take quite some time, during which [`RNiftyReg`
package](https://github.com/jonclayden/RNiftyReg) is constructing the
best transformation of the modified image back on to the original. The
result of `ms_rectify_maps()` is a spatial object in
[`sf`](https://cran.r-project.org/package=sf)-format representing the
convex hulls constructed around the red lines shown in the above map.
Note that the function automatically distinguishes the two hulls.
Finally, we can plot the result as an interactive map using packages
like [`mapdeck`](https://github.com/symbolixAU/mapdeck), or
[`mapview`](https://github.com/r-spatial/mapview):

![](./man/figures/leaflet-1.png)

And our hand-drawn lines shown above has been converted to standard
spatial objects able to be analysed in any desired way.
