# get name of png file, converting pdf to png if neccesary
get_map_png <- function (mapfile, quiet = TRUE)
{
    png_name <- paste0 (tools::file_path_sans_ext (mapfile), ".png")
    if (!(file.exists (mapfile) | file.exists (png_name)))
        stop ("Neither ", mapfile, " nor ", png_name, " exist")

    if (!file.exists (png_name))
        pdf_to_png (mapfile) # nocov

    if (file.size (mapfile) > 1e6)
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
    chk <- file.copy (mapfile, newname)
    s <- file.size (newname)
    # % reduction to resize to 1MB:
    red <- paste0 (floor (100 / (s / 1e6)), "%")
    img <- magick::image_read (newname)
    bbox <- magick::image_comment (img)
    magick::image_resize (img, geometry = red) %>%
        magick::image_write (path = newname, comment = bbox)

    if (!quiet)
    {
        snew <- formatC (file.size (newname)/ 1e6, format = "f", digits = 1)
        message ("\r", cli::symbol$tick, " Reduced size of '", mapfile,
                 "' of ", smb, "MB to ", snew, "MB")
    }

    return (newname)
}
# nocov end

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
    bbox <- magick::image_read (fname) %>%
        magick::image_comment ()
    # change "EX" at start of file comment to "TX" to flag trimmed:
    if (substring (bbox, 1, 1) != "T")
    {
        # nocov start
        # -- sample images have already been trimmed, so can't be tested
        img <- magick::image_read (fname) %>%
            magick::image_trim (fuzz = 1)
        bbox <- paste0 ("T", substring (bbox, 2, nchar (bbox)))
        magick::image_write (img, path = fname, comment = bbox)
        # nocov end
    }

    return (fname)
}
