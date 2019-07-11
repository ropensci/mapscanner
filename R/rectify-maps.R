#' ms_rectify_maps
#'
#' Scan in two pdf or png maps, rectify them with `RNiftyReg`, and return the
#' modifications in `map_modified` as spatial objects in \pkg{sf} format.
#'
#' @param map_original File name of the original map without anything drawn over
#' it (either a `.pdf` or `.png`; extension will be ignored).
#' @param map_modified File name of the modified version with drawings (either a
#' `.pdf` or `.png`; extension will be ignored).
#' @param non_linear Integer value of 0, 1, or 2 representing degree of
#' non-linearity in modified image - see Note.
#' @param type Currently either "points", "polygons", or "hulls", where
#' "points" simply reduces each distinct object to a single, central point;
#' "polygons" identifies individual groups and returns the polygon representing
#' the outer boundary of each; and "hulls" constructs convex polygons around
#' each group.
#' @param downsample Factor by which to downsample polygons, noting that
#' polygons initially include every outer pixel of image, so can generally be
#' downsampled by at least an order or magnitude (`n = 10`). Higher values may
#' be used for higher-resolution images; lower values will generally only be
#' necessary for very low lower resolution images.
#' @param quiet If `FALSE`, display progress information on screen
#' @return An \pkg{sf} object representing the drawn additions to map_modified.
#'
#' The `non-linear` parameter should generally set according to how the modified
#' maps were digitised. A value of 0 will give fastest results, and should be
#' used for directly scanned or photocopied images. A value of 1 (the default)
#' still presumes modified images have been linearly translated, and will apply
#' affine transformations (rotations, contractions, dilations). This value
#' should be used when modified images have been photographed (potentially from
#' an oblique angle). A value of 2 should only be used when modified maps have
#' somehow been non-linearly distorted, for example through having been crumpled
#' or screwed up. Rectification with `non-linear = 2` will likely take
#' considerably longer than with lower values.
#'
#' @examples
#' f_orig <- system.file ("extdata", "omaha.png", package = "mapscanner")
#' f_mod <- system.file ("extdata", "omaha-polygons.png", package = "mapscanner")
#' \dontrun{
#' xy_hull <- ms_rectify_maps (f_orig, f_mod, type = "hull")
#' xy_poly <- ms_rectify_maps (f_orig, f_mod, type = "polygons")
#' xy_pts <- ms_rectify_maps (f_orig, f_mod, type = "points")
#' }
#' # reduce file sizes to 1/4 to speed up these examples:
#' f_orig2 <- file.path (tempdir (), "omaha.png")
#' f_modified2 <- file.path (tempdir (), "omaha-polygons.png")
#' magick::image_read (f_orig) %>%
#'     magick::image_resize ("25%") %>%
#'     magick::image_write (f_orig2)
#' magick::image_read (f_mod) %>%
#'     magick::image_resize ("25%") %>%
#'     magick::image_write (f_modified2)
#'
#' # then rectify those files:
#' xy_hull <- ms_rectify_maps (f_orig2, f_modified2, type = "hull")
#' xy_poly <- ms_rectify_maps (f_orig2, f_modified2, type = "polygons")
#' xy_pts <- ms_rectify_maps (f_orig2, f_modified2, type = "points")
#'
#' @export
ms_rectify_maps <- function (map_original, map_modified, non_linear = 1,
                             type = "polygons", downsample = 10, quiet = FALSE)
{
    non_linear <- non_lin_to_scope (non_linear)

    type <- match.arg (type, c ("points", "polygons", "hulls"))
    if (type != "polygons" && downsample != 10)
        message ("downsample is only used for polygons")

    map_original <- get_map_png (map_original)
    map_modified <- get_map_png (map_modified)

    f_orig <- trim_white (map_original)
    f_mod <- trim_white (map_modified)
    map <- png::readPNG (f_orig)
    map_scanned <- png::readPNG (f_mod)

    # niftyreg (source, target) transforms source into the space of target, and
    # returns $image as "the registered and resampled 'source' image in the
    # space of the 'target' image"
    if (!quiet)
    {
        message (cli::rule (left = "mapscanner", line = 2, col = "green"))
        message (cli::symbol$pointer, " rectifying the two maps ", appendLF = FALSE)
    }
    res <- m_niftyreg (map_scanned, map, non_linear = non_linear)
    if (!quiet)
    {
        message ("\r", cli::symbol$tick, " rectifying the two maps ")

        message (cli::symbol$pointer, " extracting drawn objects ", appendLF = FALSE)
    }
    img <- extract_channel (res)
    if (!quiet)
    {
        message ("\r", cli::symbol$tick, " extracting drawn objects ")
        message (cli::symbol$pointer, " converting to spatial format ", appendLF = FALSE)
    }
    res <- rectify_channel (img, f_orig, type = type, n = downsample)
    if (!quiet)
        message ("\r", cli::symbol$tick, " converting to spatial format ")
    return (res)
}

# Convert [1,2,3] non_linear param to niftyreg scope values
non_lin_to_scope <- function (non_linear)
{
    if (!(is.numeric (non_linear) & length (non_linear) == 1))
        stop ("non_linear must be a single integer value")
    if (!non_linear %in% 0:2)
        stop ("non_linear must be a value of 0, 1, or 2")

    c ("rigid", "affine", "nonlinear") [non_linear + 1]
}

m_niftyreg <- memoise::memoise (function (map_scanned, map, non_linear)
    RNiftyReg::niftyreg (map_scanned, map, scope = non_linear))


# extract any non-greyscale components from RNiftyReg output
# nr is result of scanmaps, output from RNiftyReg
extract_channel <- function (nr)
{
    img <- nr$image # house and img have 3 layers [r, g, b]
    img <- round (img * 100) / 100

    get1layer <- function (img, i) {
        res <- img [, , i]
        ifelse (res < 0.5, 0, 1)    }
    img_r <- get1layer (img, 1)
    img_b <- get1layer (img, 2)
    img_g <- get1layer (img, 3)

    delta <- abs (img_r - img_b) +
        abs (img_r - img_b) +
        abs (img_b - img_g)

    index <- which (delta > 0)
    res <- array (0, dim = dim (img_r))
    res [index] <- 1

    return (res)
}

# origin is the raster image, channel is result of extract_channel
rectify_channel <- function (channel, original, type, n = 10)
{
    crs_from <- "+proj=merc +a=6378137 +b=6378137"
    crs_to <- 4326

    bbox <- bbox_from_png (original)

    if (type == "hulls")
    {
        hulls <- polygon_hulls (channel)
        # Only need to flip the y-axis here:
        hulls <- lapply (hulls, function (i) {
                             i$y <- nrow (channel) - i$y
                             i$x <- ((i$x - 1) / (ncol (channel) - 1))
                             i$y <- ((i$y - 1) / (nrow (channel) - 1))
                             return (i)    })

        # Then scale to bbox and convert to st_polygon
        hulls <- lapply (hulls, function (i) {
                                  i$x <- bbox [1] + i$x * (bbox [3] - bbox [1])
                                  i$y <- bbox [2] + i$y * (bbox [4] - bbox [2])
                                  sf::st_polygon (list (as.matrix (i)))
                    })
        geometry <- sf::st_sfc (hulls, crs = crs_from)
    } else # make polygons
    {
        boundaries <- polygon_boundaries (channel)
        # boundaries then need to be rotated, so first lines impelement:
        # x <- y
        # y <- nrow (channel) - x
        boundaries <- lapply (boundaries, function (i) {
                                  temp <- i$x
                                  i$x <- i$y
                                  i$y <- nrow (channel) - temp
                                  i$x <- ((i$x - 1) / (ncol (channel) - 1))
                                  i$y <- ((i$y - 1) / (nrow (channel) - 1))
                                  return (i)    })
        # Then scale to bbox and convert to st_polygon
        boundaries <- lapply (boundaries, function (i) {
                                  i <- smooth_polygon (i, n = n)
                                  i$x <- bbox [1] + i$x * (bbox [3] - bbox [1])
                                  i$y <- bbox [2] + i$y * (bbox [4] - bbox [2])
                                  sf::st_polygon (list (as.matrix (i)))
                    })
        geometry <- sf::st_sfc (boundaries, crs = crs_from)
        if (type == "points")
        {
            geometry <- sf::st_centroid (geometry)
        }
    }

    # Then re-project:
    sf::st_sf (geometry = geometry) %>%
        sf::st_transform (crs = crs_to)
}

# img is a channel
polygon_boundaries <- function (img)
{
    img <- matrix (as.logical (img), nrow = nrow (img))
    cmat <- rcpp_components (img)
    comps <- seq (1:max (cmat))

    boundaries <- list ()
    for (ci in comps)
    {
        cmat_i <- cmat
        cmat_i [cmat_i != ci] <- 0
        i1 <- get_shape_index (cmat_i, x = TRUE)
        i2 <- get_shape_index (cmat_i, x = FALSE)

        cmat_i <- cmat_i [i1, i2]
        xy <- rcpp_boundary (cmat_i)
        xy$x <- xy$x + min (i1) # no - 1, because first row is blank
        xy$y <- xy$y + min (i2)

        boundaries [[length (boundaries) + 1]] <- xy
    }
    return (boundaries)
}

get_shape_index <- function (cmat, x = TRUE)
{
    if (x)
        index <- which (rowSums (cmat) > 0)
    else
        index <- which (colSums (cmat) > 0)
    index <- seq (min (index) - 1, max (index) + 1)
    if (x)
        index <- index [index > 0 & index < nrow (cmat)]
    else
        index <- index [index > 0 & index < ncol (cmat)]
    return (index)
}

# fit spline at every n points:
smooth_polygon <- function (xy, n = 10)
{
    n <- floor (nrow (xy) / n)
    x <- stats::spline (seq (nrow (xy)), xy$x, n = n, method = "periodic")$y
    y <- stats::spline (seq (nrow (xy)), xy$y, n = n, method = "periodic")$y
    data.frame (x = x, y = y)
}

polygon_hulls <- function (img)
{
    img <- matrix (as.logical (img), nrow = nrow (img))
    cmat <- rcpp_components (img)
    comps <- seq (1:max (cmat))

    hulls <- list ()
    for (ci in comps)
    {
        cmat_i <- cmat
        cmat_i [cmat_i != ci] <- 0

        x <- t (array (seq (ncol (img)), dim = c (ncol (img), nrow (img))))
        y <- array (seq (nrow (img)), dim = c (nrow (img), ncol (img)))

        index <- which (cmat == ci)
        xy <- data.frame (x = x [index], y = y [index])
        index <- grDevices::chull (xy)
        hulls [[length (hulls) + 1]] <- xy [c (index, index [1]), ]
    }
    return (hulls)
}
