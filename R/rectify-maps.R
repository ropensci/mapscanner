#' ms_rectify_maps
#'
#' Scan in two pdf or jpg maps, rectify them with `RNiftyReg`, and return the
#' modifications in `map_modified` as spatial objects in \pkg{sf} format.
#'
#' @param map_original File name of the original map without anything drawn over
#' it (either a `.pdf` or `.jpg`; extension will be ignored).
#' @param map_modified File name of the modified version with drawings (either a
#' `.pdf` or `.jpg`; extension will be ignored).
#' @return An \pkg{sf} object representing the drawn additions to map_modified.
#'
#' @note Currently only return a single convex polygon surrounding all elements
#' added to `map_modified`.
#'
#' @export
ms_rectify_maps <- function (map_original, map_modified)
{
    map_original <- get_map_jpg (map_original)
    map_modified <- get_map_jpg (map_modified)

    f_orig <- trim_white (map_original)
    f_mod <- trim_white (map_modified)
    map <- jpeg::readJPEG (f_orig)
    map_scanned <- jpeg::readJPEG (f_mod)

    # niftyreg (source, target) transforms source into the space of target, and
    # returns $image as "the registered and resampled 'source' image in the
    # space of the 'target' image"
    res <- RNiftyReg::niftyreg (map_scanned, map)

    img_r <- extract_channel_r (res)
    rectify_channel (img_r, f_orig)
}

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
rectify_channel <- function (channel, original)
{
    bbox <- bbox_from_jpg (original)
    x <- seq (bbox [1], bbox [3], length.out = ncol (channel))
    y <- rev (seq (bbox [2], bbox [4], length.out = nrow (channel)))
    x <- t (array (x, dim = c (ncol (channel), nrow (channel))))
    y <- array (y, dim = c (nrow (channel), ncol (channel)))

    x <- x [which (channel == 1)]
    y <- y [which (channel == 1)]

    index <- grDevices::chull (x, y)
    xy <- cbind (x, y) [c (index, index [1]), ]
    xy <- sf::st_polygon (list (xy))

    # Then re-project:
    crs_from <- "+proj=merc +a=6378137 +b=6378137"
    crs_to <- 4326
    sf::st_sf (geometry = sf::st_sfc (xy, crs = crs_from)) %>%
        sf::st_transform (crs = crs_to)
}