
pdf_to_jpg <- function (file)
{
    file <- paste0 (tools::file_path_sans_ext (file), ".pdf")
    if (!file.exists (file))
        stop ("file ", file, " does not exist")

    bb <- bbox_from_pdf (file, asString = TRUE)

    fout <- paste0 (tools::file_path_sans_ext (file), ".jpg")
    pdftools::pdf_convert (file, format = "jpg", filenames = fout)
    img <- magick::image_read (fout)
    magick::image_write (img, path = fout, comment = bb)
}

bbox_from_pdf <- function (file, asString = FALSE)
{
    file <- paste0 (tools::file_path_sans_ext (file), ".pdf")
    if (!file.exists (file))
        stop ("file ", file, " does not exist")
    bbox <- pdftools::pdf_info (file)$keys$Title
    if (!asString)
    {
        bbox <- strsplit (bbox, "\\+") [[1]]
        bbox [1] <- substring (bbox [1], 3, nchar (bbox [1])) # rm "EX"
        bbox <- as.numeric (bbox)
    }
    return (bbox)
}

# Convert input fname as pdf to jpg and trim white space from border
trim_white <- function (fname)
{
    fname <- paste0 (tools::file_path_sans_ext (fname), ".pdf")
    if (!file.exists (fname))
        stop ("file ", fname, " does not exist")
    fname <- pdftools::pdf_convert (fname, format = "jpg")

    magick::image_read (fname) %>%
        magick::image_trim (fuzz = 1) %>%
        magick::image_write (fname)
    return (fname)
}

#' scan_maps
#'
#' Scan in two pdf maps and rectify them with `RNiftyReg`
#' @param map_original The original map without anything drawn over it.
#' @param map_modified The modified version with drawings
#' @return An \pkg{RNiftyReg} object
#' @export
scan_maps <- function (map_original, map_modified)
{
    f1 <- trim_white (map_original)
    f2 <- trim_white (map_modified)
    map <- jpeg::readJPEG (f1)
    map_scanned <- jpeg::readJPEG (f2)

    # niftyreg (source, target) transforms source into the space of target, and
    # returns $image as "the registered and resampled 'source' image in the
    # space of the 'target' image"
    RNiftyReg::niftyreg (map_scanned, map)
}
