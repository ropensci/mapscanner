
#' ms_rectify_maps
#'
#' Rectify two previously scanned-in pdf or png maps with `RNiftyReg`, and
#' return the modifications in `map_modified` as spatial objects in \pkg{sf}
#' format.
#'
#' @param map_original File name of the original map without anything drawn over
#' it (either a `.pdf` or `.png`; extension will be ignored).
#' @param map_modified File name of the modified version with drawings (either a
#' `.pdf` or `.png`; extension will be ignored).
#' @param nitems Optional parameter to explicitly specify number of distinct
#' items to be extracted from map; if possible, specifying this parameter may
#' improve results.
#' @param non_linear Integer value of 0, 1, or 2 representing degree of
#' non-linearity in modified image - see Note.
#' @param type Currently either "points", "polygons", or "hulls", where
#' "points" simply reduces each distinct object to a single, central point;
#' "polygons" identifies individual groups and returns the polygon representing
#' the outer boundary of each; and "hulls" constructs convex or concave polygons
#' around each group.
#' @param downsample Factor by which to downsample `type = "polygons"`, noting
#' that polygons initially include every outer pixel of image, so can generally
#' be downsampled by at least an order or magnitude (`n = 10`). Higher values
#' may be used for higher-resolution images; lower values will generally only be
#' necessary for very low lower resolution images.
#' @param concavity For `type = "hulls"`, a value between 0 and 1, with 0 giving
#' convex hulls and 1 giving highly concave hulls.
#' @param length_threshold For `type = "hulls"`, the minimal length of a segment
#' to be made more convex. Low values will produce highly detailed hulls which
#' may cause problems; if in doubt, or if odd results appear, increase this
#' value.
#' @param quiet If `FALSE`, display progress information on screen
#' @return An \pkg{sf} object representing the drawn additions to map_modified.
#'
#' @note The `non-linear` parameter should generally set according to how the
#' modified maps were digitised. A value of 0 will give fastest results, and
#' should be used for directly scanned or photocopied images. A value of 1 (the
#' default) still presumes modified images have been linearly translated, and
#' will apply affine transformations (rotations, contractions, dilations). This
#' value should be used when modified images have been photographed (potentially
#' from an oblique angle). A value of 2 should only be used when modified maps
#' have somehow been non-linearly distorted, for example through having been
#' crumpled or screwed up. Rectification with `non-linear = 2` will likely take
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
ms_rectify_maps <- function (map_original, map_modified, nitems = NULL,
                             non_linear = 1, type = "hulls", downsample = 10,
                             concavity = 0, length_threshold = 10,
                             quiet = FALSE)
{
    non_linear <- non_lin_to_scope (non_linear)

    type <- match.arg (type, c ("points", "polygons", "hulls"))
    if (type != "polygons" && downsample != 10)
        message ("downsample is only used for polygons")

    concavity <- check_concavity (concavity)
    length_threshold <- check_threshold (length_threshold)

    if (!quiet)
        message (cli::rule (left = "mapscanner", line = 2, col = "green"))

    map_original <- get_map_png (map_original, quiet = quiet)
    map_modified <- get_map_png (map_modified, quiet = quiet)
    chk <- check_rotation (map_original, map_modified)
    if (chk)
    {
        ms_rotate_map (map_original, map_modified) 
        stop ("file [", map_modified, "] appears to be rotated relative to [",
              map_original, "]; please rotate image using `ms_rotate_maps`")
    }

    f_orig <- trim_white (map_original)
    f_mod <- trim_white (map_modified)
    f_orig <- reduce_image_size (f_orig, quiet = quiet)
    f_mod <- reduce_image_size (f_mod, quiet = quiet)
    map <- png::readPNG (f_orig)
    map_scanned <- png::readPNG (f_mod)

    # niftyreg (source, target) transforms source into the space of target, and
    # returns $image as "the registered and resampled 'source' image in the
    # space of the 'target' image"
    if (!quiet)
    {
        message (cli::symbol$pointer, " Rectifying the two maps ",
                 appendLF = FALSE)
    }
    nr <- m_niftyreg (map_scanned, map, non_linear = non_linear)
    if (!quiet)
        message ("\r", cli::symbol$tick, " Rectified the two maps  ")

    img <- extract_channel (nr, nitems = nitems, quiet = quiet)
    check_img_sanity (img)

    res <- rectify_channel (img, f_orig, type = type, n = downsample,
                            concavity = concavity,
                            length_threshold = length_threshold,
                            quiet = quiet)
    if (!quiet)
        message ("\r", cli::symbol$tick, " Converted to spatial format ")
    return (res)
}

check_concavity <- function (concavity)
{
    if (!(is.numeric (concavity) | length (concavity) > 1))
        stop ("concavity must be numeric")
    if (concavity < 0 | concavity > 1)
    {
        message ("concavity must be between 0 and 1; setting to default of 0")
        concavity <- 0
    }
    return (concavity)
}

check_threshold <- function (length_threshold)
{
    if (!(is.numeric (length_threshold) | length (length_threshold) > 1))
        stop ("length_threshold must be numeric")
    if (length_threshold < 1)
    {
        message ("length_threshold must be >= 1")
        length_threshold <- 10
    }
    return (length_threshold)
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
# threshold is based on delta, which is the aggregate difference between colour
# channels. Values above (threhold * median (delta)) are considered to be a
# single colour and are kept for futher analysis; all other values are removed.
extract_channel <- function (nr, nitems = NULL, quiet = FALSE)
{
    img <- nr$image # house and img have 3 layers [r, g, b]
    x <- get_channel_diff_threshold (img, nitems = nitems, quiet = quiet)
    if (is.null (nitems))
        nitems <- x$ncomps
    cmat <- get_component_mat (img, x$threshold)
    tc <- table (cmat [cmat > 0])
    nitems <- length (which (tc > 4))
    if (!quiet)
        message (cli::symbol$tick, " Identified ", nitems, " objects")
    comp_nums <- as.integer (names (sort (tc, decreasing = TRUE)) [1:nitems])
    cmat [which (!cmat %in% comp_nums)] <- 0
    cmat [which (cmat > 0)] <- 1 # same as %in% comp_nums, but more efficient

    cmat <- t (apply (cmat, 2, rev))

    return (cmat)
}

# threshold is for aggregate difference between the 3 channels, used to
# distinguish coloured from grey-scale pixels. Higher values extract fewer
# pixels.
get_component_mat <- function (img, threshold)
{
    res <- array (0, dim = c (dim (img) [1], dim (img) [2]))

    delta <- abs (img [, , 1] - img [, , 2]) +
        abs (img [, , 1] - img [, , 2]) +
        abs (img [, , 2] - img [, , 3])
    index <- which (delta >= (threshold * mean (delta)))
    res [index] <- 1

    rcpp_components (res)
}

# terminology note: user interface refers to "items", but code itself identifies
# connected components, so internal code uses "components" or "comps", while
# "nitems" is the user-exposed parameter.
get_channel_diff_threshold <- function (img, nitems = NULL, quiet = FALSE)
{
    # count number of pixels in specified number of components to numbers of
    # pixels above threshold but not in those components.
    count_comp_pixels <- function (img, threshold, ncomps) {
        cmat <- get_component_mat (img, threshold = threshold)
        tc <- table (cmat [cmat > 0])
        n_in <- sum (sort (tc, decreasing = TRUE) [1:ncomps])
        n_out <- sum (tc) - n_in
        c (threshold = threshold, n_in = n_in, n_out = n_out)
    }

    if (!quiet)
        message (cli::symbol$pointer,
                 " Estimating optimal signal-to-noise threshold",
                 appendLF = FALSE)
    if (is.null (nitems))
    {
        nitems <- get_num_components (img)
        thr0 <- nitems$threshold
        nitems <- nitems$ncomps
    } else
    {
        # in this case, do an initial coarse search, which is simply repeated
        # below over a more refined range.
        thr <- c (1, ceiling (2 ^ ((2:10) / 2)))
        np <- vapply (thr, function (i) count_comp_pixels (img, i, nitems),
                      numeric (3))
        np <- data.frame (t (np))
        thr0 <- thr [which.max (np$n_in / np$n_out)] # works also for n_out == 0
    }

    thr <- thr0 + (-4:5) * thr0 / 5
    np <- vapply (thr, function (i) count_comp_pixels (img, i, ncomps = nitems),
                  numeric (3))
    np <- data.frame (t (np))
    thr0 <- thr [which.max (np$n_in / np$n_out)] # works also for n_out == 0
    if (!quiet)
        message ("\r", cli::symbol$tick,
                 " Estimated optimal signal-to-noise threshold")

    list (ncomps = nitems, threshold = thr0)
}

get_num_components <- function (img)
{
    num_comps <- function (img, thr = 1) {
        cmat <- get_component_mat (img, threshold = thr)
        length (table (cmat [cmat > 0]))
    }
    # Number of components extracted decreases with increasing threshold,
    # generally to a local minimum which is taken as the initial threshold to be
    # used, before increasing again due to adding extra noise.
    thr <- c (1, ceiling (2 ^ ((2:10) / 2)))
    n <- sapply (thr, function (i) num_comps (img, thr = i))
    nf <- stats::filter (n, rep (1, 3) / 3)
    n [which (!is.na (nf))] <- nf [which (!is.na (nf))]
    # find first concave region
    dn <- diff (n)
    # do no consider any initial positive values
    index <- which (dn > 0)
    if (length (index) == 0) # all thresholds give same #comps
    {
        thr0 <- thr [1]
        n <- n [1]
    } else {
        if (index [1] == 1)
        {
            index <- index [c (1, which (diff (index) == 1) + 1)]
            dn [index] <- -1 # arbitrary negative value for next step
        }
        if (all (dn < 0))
            thr0 <- thr [which.min (dn)] # in case no local min
        else
            thr0 <- thr [which (dn > 0) [1]]

        # estimate number of components for that threshold
        cmat <- get_component_mat (img, threshold = thr0)
        tc <- table (cmat [cmat > 0])
        n <- as.integer (names (table (tc)))
        # n is then sizes of components in increasing order. The first "real"
        # component is presumed to be after the first jump in size of > 2
        n <- length (which (diff (n) > 2))
    }

    list (ncomps = n, threshold = thr0)
}

# currently implements only one sanity check for feature extraction failure,
# which is whether the number of feature pixels > number of background pixels
check_img_sanity <- function (img)
{
    if (length (which (img == 1)) > length (which (img == 0)))
        stop ("Items unable to be extracted from map; perhaps try ",
              "explicitly specifying 'nitems'?")
}

# origin is the raster image, channel is result of extract_channel
rectify_channel <- function (channel, original, type, n = 10,
                             concavity, length_threshold, quiet = TRUE)
{
    crs_from <- "+proj=merc +a=6378137 +b=6378137"
    crs_to <- 4326

    bbox <- bbox_from_png (original)

    if (type == "hulls")
    {
        if (!quiet)
            message (cli::symbol$pointer, " Converting to spatial format ",
                     appendLF = FALSE)
        if (concavity == 0) {
            hulls <- polygon_hulls (channel, as_index = FALSE)
            hulls <- lapply (hulls, function (i) {
                                 i$x <- ((i$x - 1) / (ncol (channel) - 1))
                                 i$y <- ((i$y - 1) / (nrow (channel) - 1))
                                 return (i)    })
        } else # concaveman
        {
            hulls <- polygon_hulls (channel, as_index = TRUE)
            # values -> 0 translate to large concaveman values;
            # values -> 1 translate to concaveman values of 1
            concavity <- max (concavity, 1e-6)
            concavity <- 1 / concavity
            hulls <- lapply (hulls, function (i) {
                                 res <- rcpp_concaveman (i$xy, i$index - 1,
                                                         concavity,
                                                         length_threshold)
                                 rbind (res, res [1, ])
                    })
            hulls <- lapply (hulls, function (i) {
                                 i$x <- ((i$x - 1) / (nrow (channel) - 1))
                                 i$y <- ((i$y - 1) / (ncol (channel) - 1))
                                 return (i)    })
        }

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
        boundaries <- lapply (boundaries, function (i) {
                                  i$x <- ((i$x - 1) / (nrow (channel) - 1))
                                  i$y <- ((i$y - 1) / (ncol (channel) - 1))
                                  return (i)    })
        # remove any boundaries with < 4 points
        lens <- vapply (boundaries, nrow, integer (1))
        if (any (lens < 4))
        {
            nrm <- length (which (lens < 4))
            txt <- ifelse (nrm == 1, "item", "items")
            message (cli::symbol$tick, " removed ", nrm,
                     " anomalously detected ", txt)
            boundaries <- boundaries [which (lens > 3)]
        }
        if (!quiet)
            message (cli::symbol$pointer, " Converting to spatial format ",
                     appendLF = FALSE)
        # Then scale to bbox and convert to st_polygon. smooth boundary polygons
        # that have > (10 * n = downsample) points
        boundaries <- lapply (boundaries, function (i) {
                                  if (nrow (i) > (10 * n))
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

polygon_hulls <- function (img, as_index = FALSE)
{
    img <- matrix (as.logical (img), nrow = nrow (img))
    cmat <- rcpp_components (img)
    comps <- seq (1:max (cmat))

    hulls <- list ()
    for (ci in comps)
    {
        cmat_i <- cmat
        cmat_i [cmat_i != ci] <- 0

        y <- t (array (seq (ncol (img)), dim = c (ncol (img), nrow (img))))
        x <- array (seq (nrow (img)), dim = c (nrow (img), ncol (img)))

        index <- which (cmat == ci)
        # only make hulls around > 4 points:
        if (length (index) > 4)
        {
            xy <- data.frame (x = x [index], y = y [index])
            index <- grDevices::chull (xy)
            if (as_index)
                hulls [[length (hulls) + 1]] <- list (xy = xy, index = index)
            else
                hulls [[length (hulls) + 1]] <- xy [c (index, index [1]), ]
        }
    }
    return (hulls)
}
