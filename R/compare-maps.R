
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
    ex <- attributes (raster::extent (original))
    x <- seq (ex$xmin, ex$xmax, length.out = ncol (channel))
    y <- rev (seq (ex$ymin, ex$ymax, length.out = nrow (channel)))
    x <- t (array (x, dim = c (ncol (channel), nrow (channel))))
    y <- array (y, dim = c (nrow (channel), ncol (channel)))

    x <- x [which (channel == 1)]
    y <- y [which (channel == 1)]

    index <- chull (x, y)
    xy <- cbind (x, y) [c (index, index [1]), ]
    xy <- sf::st_polygon (list (xy))

    # Then re-project:
    crs <- attr (raster::crs (original), "projargs")
    sf::st_sf (geometry = sf::st_sfc (xy, crs = crs)) %>%
        sf::st_transform (crs = 4326)
}
