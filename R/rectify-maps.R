#' ms_rectify_maps
#'
#' Scan in two pdf or jpg maps, rectify them with `RNiftyReg`, and return the
#' modifications in `map_modified` as spatial objects in \pkg{sf} format.
#'
#' @param map_original File name of the original map without anything drawn over
#' it (either a `.pdf` or `.jpg`; extension will be ignored).
#' @param map_modified File name of the modified version with drawings (either a
#' `.pdf` or `.jpg`; extension will be ignored).
#' @param type Currently either "points", "polygons", or "hulls", where
#' "points" simply returns the identified points that differ between the two
#' maps; "polygons" identifies individual groups and returns the polygon
#' representing the outer boundary of each; and "hulls" constructs convex
#' polygons around each group.
#' @param downsample Factor by which to downsample polygons, noting that
#' polygons initially include every outer pixel of image, so can generally be
#' downsampled by at least an order or magnitude (`n = 10`). Higher values may
#' be used for higher-resolution images; lower values will generally only be
#' necessary for very low lower resolution images.
#' @return An \pkg{sf} object representing the drawn additions to map_modified.
#'
#' @note Currently only return a single convex polygon surrounding all elements
#' added to `map_modified`.
#'
#' @export
ms_rectify_maps <- function (map_original, map_modified, type = "polygons",
                             downsample = 10)
{
    type <- match.arg (type, c ("points", "polygons", "hulls"))
    if (type != "polygons" && downsample != 10)
        message ("downsample is only used for polygons")

    map_original <- get_map_jpg (map_original)
    map_modified <- get_map_jpg (map_modified)

    f_orig <- trim_white (map_original)
    f_mod <- trim_white (map_modified)
    map <- jpeg::readJPEG (f_orig)
    map_scanned <- jpeg::readJPEG (f_mod)

    # niftyreg (source, target) transforms source into the space of target, and
    # returns $image as "the registered and resampled 'source' image in the
    # space of the 'target' image"
    res <- m_niftyreg (map_scanned, map)

    img_r <- extract_channel_r (res)
    rectify_channel (img_r, f_orig, type = type, n = downsample)
}

m_niftyreg <- memoise::memoise (function (map_scanned, map)
    RNiftyReg::niftyreg (map_scanned, map))


# extract unique bits of red channel from RNiftyReg output
# nr is result of scanmaps, output from RNiftyReg
extract_channel_r <- function (nr)
{
    img <- nr$image # house and img have 3 layers [r, g, b]
    img <- round (img * 100) / 100

    get1layer <- function (img, i) {
        res <- img [, , i]
        ifelse (res < 0.5, 0, 1)    }
    img_r <- get1layer (img, 1)
    img_b <- get1layer (img, 2)
    img_g <- get1layer (img, 3)

    index_r <- which (!(img_r == img_g | img_r == img_b))

    img_r [index_r] <- 1
    img_r [!(seq (img_r) %in% index_r)] <- 0

    return (img_r)
}

# origin is the raster image, channel is result of extract_channel
rectify_channel <- function (channel, original, type, n = 10)
{
    crs_from <- "+proj=merc +a=6378137 +b=6378137"
    crs_to <- 4326

    bbox <- bbox_from_jpg (original)

    if (type == "polygons")
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
    } else if (type == "hulls")
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
    } else if (type == "points")
    {
        x <- seq (bbox [1], bbox [3], length.out = ncol (channel))
        y <- rev (seq (bbox [2], bbox [4], length.out = nrow (channel)))
        x <- t (array (x, dim = c (ncol (channel), nrow (channel))))
        y <- array (y, dim = c (nrow (channel), ncol (channel)))

        x <- x [which (channel == 1)]
        y <- y [which (channel == 1)]

        xy <- cbind (x, y)
        geometry <- sf::st_sfc (sf::st_multipoint (xy), crs = crs_from)
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
