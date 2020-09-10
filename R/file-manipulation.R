#' Rotate maps
#'
#' Display original and modified maps to determine necessary rotation
#' @inheritParams ms_rectify_map
#' @param rotation Rotation value to be applied, generally +/- 90
#' @param apply_rotation If `FALSE`, display results of rotation without
#' actually applying it; otherwise transform the specified `map_modified` image
#' according to the specified rotation.
#'
#' @note If a call to \link{ms_rectify_map} detects potential image rotation,
#' that function will stop and suggest that rotation be applied using this
#' function in order to determine the required degree of image rotation. Values
#' for `rotation` can be trialled in order to determine the correct value,
#' following which that value can be entered with `apply_rotation = TRUE` in
#' order to actually apply that rotation to the modified image.
#'
#' @export
ms_rotate_map <- function (map_original, map_modified, rotation = 0,
                           apply_rotation = FALSE)
{
    map_original <- get_map_png (map_original, quiet = TRUE)
    map_modified <- get_map_png (map_modified, quiet = TRUE)

    if (!apply_rotation)
        f <- file.path (tempdir (), "test.png")
    else
        f <- map_modified

    magick::image_read (map_modified) %>%
        magick::image_rotate (rotation) %>%
        magick::image_write (f)

    if (!apply_rotation)
    {
        requireNamespace ("mmand")
        map <- png::readPNG (map_original)
        map_scanned <- png::readPNG (f)

        graphics::par (mfrow = c (1, 2), mar = c (0, 0, 1, 0))
        graphics::plot.new ()
        mmand::display (map, add = TRUE)
        graphics::plot.new ()
        mmand::display (map_scanned, add = TRUE)
        graphics::title (main = paste0 ("rotation = ", rotation))
    }
}

# params are names of .png files
check_rotation <- function (map_original, map_modified)
{
    o <- magick::image_read (map_original) %>%
        magick::image_info ()
    o_w2h <- o$width / o$height
    m <- magick::image_read (map_modified) %>%
        magick::image_info ()
    m_w2h <- m$width / m$height

    rotated <- FALSE
    if (sign (1 - o_w2h) != sign (1 - m_w2h))
        rotated <- TRUE # nocov

    return (rotated)
}

# get name of png file, converting pdf to png if neccesary
get_map_png <- function (mapfile, quiet = TRUE)
{
    png_name <- paste0 (tools::file_path_sans_ext (mapfile), ".png")
    if (!(file.exists (mapfile) | file.exists (png_name)))
        stop ("Neither ", mapfile, " nor ", png_name, " exist")

    if (!file.exists (png_name))
        pdf_to_png (mapfile) # nocov

    if (file.size (png_name) > 1e6)
    {
        png_name <- reduce_size (png_name, quiet = quiet) # nocov
    }
    return (png_name)
}

# nocov start
# the following 2 functions are not currently tested
pdf_to_png <- function (file)
{
    file <- paste0 (tools::file_path_sans_ext (file), ".pdf")
    if (!file.exists (file))
        stop ("file ", file, " does not exist")

    bb <- bbox_from_pdf (file, as_string = TRUE)

    fout <- paste0 (tools::file_path_sans_ext (file), ".png")
    pdftools::pdf_convert (file, format = "png", filenames = fout)
    img <- magick::image_read (fout)
    magick::image_write (img, path = fout, comment = bb)
}

hash <- function (len = 10)
{
    sample (c (letters, LETTERS, 0:9), len, replace = TRUE) %>%
        paste0 (collapse = "")
}

# nocov start
reduce_size <- function (mapfile, quiet = TRUE)
{
    s <- file.size (mapfile)
    if (!quiet)
    {
        smb <- formatC (s / 1e6, format = "f", digits = 1)
        message (cli::symbol$pointer, " Reducing size of '", mapfile,
                 "' of ", smb, "MB", appendLF = FALSE)
    }

    newname <- file.path (tempdir (), paste0 ("img", hash (10), ".png"))
    invisible (file.copy (mapfile, newname))
    s <- file.size (newname)
    # % reduction to resize to 1MB:
    red <- paste0 (floor (100 / (s / 1e6)), "%")
    img <- magick::image_read (newname)
    bbox <- magick::image_comment (img)
    magick::image_resize (img, geometry = red) %>%
        magick::image_write (path = newname, comment = bbox)

    if (!quiet)
    {
        snew <- formatC (file.size (newname) / 1e6, format = "f", digits = 1)
        message ("\r", cli::symbol$tick, " Reduced size of '", mapfile,
                 "' of ", smb, "MB to ", snew, "MB")
    }

    return (newname)
}
# nocov end

# maxdim is maximal pixel size in any one dimension
reduce_image_size <- function (mapfile, maxdim = 1000, quiet = FALSE)
{
    o <- magick::image_read (mapfile)
    i <- magick::image_info (o)
    maxpix <- max (c (i$width, i$height))
    newname <- mapfile # default return value
    if (maxpix > maxdim)
    {
        # new name for modified file
        newname <- file.path (tempdir (), paste0 ("img", hash (10), ".png"))
        scl <- ceiling (maxpix / maxdim)
        if (!quiet)
            message (cli::symbol$tick, " Image [", mapfile,
                     "] reduced in size by factor of ", scl)
        dims <- paste0 (ceiling (c (i$width, i$height) / scl), collapse = "x")
        bbox <- magick::image_comment (o)
        magick::image_resize (o, geometry = dims) %>%
            magick::image_write (newname, comment = bbox)
    }
    return (newname)
}

bbox_from_pdf <- function (file, as_string = FALSE)
{
    file <- paste0 (tools::file_path_sans_ext (file), ".pdf")
    if (!file.exists (file))
        stop ("file ", file, " does not exist")
    bbox <- pdftools::pdf_info (file)$keys$Title    # nolint
    if (!as_string)
    {
        bbox <- strsplit (bbox, "\\+") [[1]]
        bbox [1] <- substring (bbox [1], 3, nchar (bbox [1])) # rm "EX"
        bbox <- as.numeric (bbox)
    }
    return (bbox)
}
# nocov end

bbox_from_png <- function (file)
{
    img <- magick::image_read (file)
    bbox <- magick::image_comment (img)
    bbox <- strsplit (bbox, "\\+") [[1]]
    bbox [1] <- substring (bbox [1], 3, nchar (bbox [1])) # rm "EX"
    as.numeric (bbox)
}

# trim white space from border of png images
trim_white <- function (fname)
{
    i <- magick::image_read (fname)
    bbox <- magick::image_comment (i)
    # change "EX" at start of file comment to "TX" to flag trimmed:
    if (substring (bbox, 1, 1) != "T")
    {
        # nocov start
        # -- sample images have already been trimmed, so can't be tested
        dims <- magick::image_info (i)
        dims0 <- as.integer (dims [c ("width", "height")])
        img <- magick::image_trim (i, fuzz = 1)
        dims <- magick::image_info (img)
        dims1 <- as.integer (dims [c ("width", "height")])
        if (identical (dims0, dims1))
            warning ("Attempt to trim white space from image appears to have ",
                     "failed - result may not be reliable. Please manually ",
                     "trim whitespace from [", fname, "] to improve results")
        else {
            bbox <- paste0 ("T", substring (bbox, 2, nchar (bbox)))
            magick::image_write (img, path = fname, comment = bbox)
        }
        # nocov end
    }

    return (fname)
}
