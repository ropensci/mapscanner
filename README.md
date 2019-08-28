<!-- README.md is generated from README.Rmd. Please edit that file -->

# mapscanner ![](http://www.textfiles.com/underconstruction/CoColosseumHoop5020underconstruction_blk.gif)

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/mpadge/mapscanner.svg?branch=master)](https://travis-ci.org/mpadge/mapscanner)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/mpadge/mapscanner?branch=master&svg=true)](https://ci.appveyor.com/project/mpadge/mapscanner)
[![codecov](https://codecov.io/gh/mpadge/mapscanner/branch/master/graph/badge.svg)](https://codecov.io/gh/mpadge/mapscanner)
[![Project Status:
Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

R package to print maps, draw on them, scan them back in, and convert to
spatial objects. Currently [under
review](https://github.com/ropensci/software-review/issues/330#event-2513283441)
at [rOpenSci](https://ropensci.org).

## installation

`mapscanner` is not (yet) on CRAN. The development version can be
installed with

``` r
remote::install_github("mpadge/mapscanner")
```

The package can then be loaded for usage in a R session with

``` r
library (mapscanner)
```

## usage

### Mapbox API tokens

Map generation with `mapscanner` requires a personal token or key from
[`mapbox`](https://mapbox.com), which can be obtained by following the
links from
[https://docs.mapbox.com/api](https://docs.mapbox.com/api/#access-tokens-and-token-scopes).
If you already have a token, the easiest way to use it with `mapscanner`
is to create (or edit) a file `~/.Renviron`, and insert a line,

``` bash
MAPBOX_TOKEN=<my_mapbox_token>
```

This will then be available every time you start R, without any need to
explicitly set the token each time you want to use the package. The
token may be given any unique name that includes “mapbox” (case
insensitive). Alternatively, if you wish to keep your token truly
private, and only use it for your current R session, you may load
`mapscanner`, and then run `set_mapbox_token(<my_mapbox_token>)`.

### Map generation

Having obtained and set a [`mapbox`](https://mapbox.com) token as
described above, `mapscanner` may then be used to generate maps. The
package comes with a sample map of Omaha, Nebraska, USA, and one with
some red lines drawn on it: ![](./man/figures/omaha-polygons.png)

That’s just a standard `png` image with no notion of geographical
coordinates. The original map was generated with

``` r
bbox <- rbind (c (-96.12923, -96.01011),
               c (41.26145, 41.32220)) # portion of omaha
ms_generate_map (bbox, max_tiles = 16L, mapname = "omaha")
```

    #> Successfully generated 'omaha.pdf' and 'omaha.png'

As indicated, the function generates a map in both `.pdf` and `.png`
formats. These files must be retained as the “master” maps against which
subsequently modified – drawn-over and scanned-in – versions will be
rectified. The `.pdf` format is generated because it will generally be
the most convenient for printing, while the rectification itself
requires `.png`-format images.

### Map rectification

The magic within the `mapscanner` package happens via the [`RNiftyReg`
package](https://github.com/jonclayden/RNiftyReg), itself primarily
intended to align brain scans and other medical images, but which is
precisely the tool needed here. The package comes with two sample `.png`
images which can be used to demonstrate map rectification. In the
following code, `f_modified` is the image shown above, modified from the
original by drawing a red line around a particular region of Omaha.

``` r
f_orig <- system.file ("extdata", "omaha.png", package = "mapscanner")
f_mod <- system.file ("extdata", "omaha-polygons.png", package = "mapscanner")
system.time (res <- ms_rectify_maps (f_orig, f_mod, type = "polygons"))
#> ══ mapscanner ═════════════════════════════════════════════════════════════
#> ❯ rectifying the two maps 
✔ rectifying the two maps 
#> ❯ extracting drawn objects 
✔ extracting drawn objects 
#> ❯ converting to spatial format 
✔ converting to spatial format
#>    user  system elapsed 
#>  50.908   0.704  13.183
res
#> Simple feature collection with 2 features and 0 fields
#> geometry type:  POLYGON
#> dimension:      XY
#> bbox:           xmin: -96.11764 ymin: 41.26657 xmax: -96.02752 ymax: 41.3008
#> epsg (SRID):    4326
#> proj4string:    +proj=longlat +datum=WGS84 +no_defs
#>                         geometry
#> 1 POLYGON ((-96.04196 41.2963...
#> 2 POLYGON ((-96.11764 41.2697...
```

The rectification can take quite some time, during which [`RNiftyReg`
package](https://github.com/jonclayden/RNiftyReg) is constructing the
best transformation of the modified image back on to the original. The
result of `ms_rectify_maps()` is a spatial object in
[`sf`](https://cran.r-project.org/package=sf)-format in which each drawn
component is represented as a separate polygon. Finally, we can plot the
result as an interactive map using packages like
[`mapdeck`](https://github.com/symbolixAU/mapdeck), or
[`mapview`](https://github.com/r-spatial/mapview):

![](./man/figures/leaflet-1.png)

And our hand-drawn lines shown above have been converted to standard
spatial objects able to be analysed in any desired way. See the [package
vignette](https://mpadge.github.io/mapscanner/articles/mapscanner.html)
for more detail of what the `mapscanner` package can do.
