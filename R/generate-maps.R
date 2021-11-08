# most of this pilfered and otherwise mildly adapted from code by @mdsumner from
# hypertidy/ceramic

#' Generate maps for 'mapscanner' use
#'
#' Generate a map image for a specified area or bounding box, by downloading
#' tiles from \url{https://www.mapbox.com/}. Map is automatically saved in both
#' `.pdf` and `.png` formats, by default in current working directory, or
#' alternative location when `mapname` includes the full path.
#'
#' @param bbox Either a string specifying the location, or a numeric bounding
#' box as a single vector of (xmin, ymin, xmax, ymax), or a 2-by-2 matrix with
#' columns of (min, max) and rows of (x, y), respectively.
#' @param max_tiles Maximum number of tiles to use to create map
#' @param mapname Name of map to be produced, optionally including full path.
#' Extension will be ignored.
#' @param bw If `FALSE`, print maps in colour, otherwise black-and-white. Note
#' that the default `style = "light"` is monochrome, and that this parameter
#' only has effect for `style` values of `"streets"` or `"outdoors"`.
#' @param style The style of the map to be generated; one of 'light', 'streets',
#' or 'outdoors', rendered in black and white. See
#' \url{https://docs.mapbox.com/api/maps/#styles/} for examples.
#' @param raster_brick Instead of automatically downloading tiles within a given
#' `bbox`, a pre-downloaded `raster::rasterBrick` object may be submitted and
#' used to generate the `.pdf` and `.png` equivalents.
#' @return Invisibly returns a `rasterBrick` object from the \pkg{raster}
#' package containing all data used to generate the map.
#'
#' @examples
#' \dontrun{
#' # code used to generate internal files for a portion of Omaha:
#' bb <- osmdata::getbb ("omaha nebraska")
#' shrink <- 0.3 # shrink that bb to 30% size
#' bb <- t (apply (bb, 1, function (i)
#'                 mean (i) + c (-shrink, shrink) * diff (i) / 2))
#' ms_generate_map (bb, max_tiles = 16L, mapname = "omaha")
#' }
#'
#' @export
ms_generate_map <- function (bbox,
                             max_tiles = 16L,
                             mapname = NULL,
                             bw = TRUE,
                             style = "light",
                             raster_brick = NULL)
{
    if (is.null (mapname))
        stop ("Please provide a 'mapname' (with optional path) ",
              "for the maps to be generated")

    style <- match.arg (tolower (style), c ("light", "streets", "outdoors"))


    if (is.null (raster_brick))
    {
        # nocov start
        # used here just to confirm token exists:
        mapbox_token <- get_mapbox_token ()
        raster_brick <- get_raster_brick (bbox = bbox,
                                          max_tiles = max_tiles,
                                          style = style,
                                          bw = bw)
        # nocov end
    }

    fname_p <- map_to_pdf (raster_brick, mapname)
    fname_j <- map_to_png (raster_brick, mapname)

    message ("Successfully generated '", fname_p, "' and '", fname_j, "'")

    invisible (raster_brick)
}

# nocov start
get_raster_brick <- function (bbox, max_tiles = 16L, style, bw)
{
    bbox <- convert_bbox (bbox)
    bbox_pair <- slippy_bbox (bbox)
    tiles <- get_tiles (bbox_pair, max_tiles = max_tiles, style = style)
    tgrid <- tiles$tiles
    br <- lapply (tiles$files, raster_brick)

    for (i in seq_along (br)) {
        br [[i]] <- raster::setExtent (br [[i]],
                                       mercator_tile_extent (tgrid$tiles$x [i],
                                                             tgrid$tiles$y [i],
                                                             zoom = tgrid$zoom))
    }

    out <- fast_merge (br)

    out <- raster::crop (out, tiles$extent, snap = "out")
    if (bw)
    {
        out_avg <- raster::stackApply (out, c (1, 1, 1), fun = mean)
        out <- raster::brick (out_avg, out_avg, out_avg)
    }

    #raster::projection (out) <- .sph_merc() ## "+proj=merc +a=6378137 +b=6378137"
    out@crs@projargs <- .sph_merc()  ## no churn through mill

    return (out)
}
# nocov end

map_to_pdf <- function (my_map, file)
{
    file <- paste0 (tools::file_path_sans_ext (file), ".pdf")

    ex <- attributes (raster::extent (my_map))
    aspect <- (ex$ymax - ex$ymin) / (ex$xmax - ex$xmin)
    A4L <- 11.7 # A4 paper is 8.3-by-11.7
    #A4H <- 8.3

    # embed extent as file name
    fname <- paste0 ("EX", ex$xmin, "+", ex$ymin, "+",
                     ex$xmax, "+", ex$ymax)

    # the following issue warnings about mode(onefile)
    suppressWarnings ({
        if (aspect < 1) {
            grDevices::pdf (my_map, width = A4L, paper = "a4r",
                                   colormodel = "gray",
                                   title = fname, file = file)
        } else {
            grDevices::pdf (my_map, height = A4L, paper = "a4",     # nocov
                                   colormodel = "gray",             # nocov
                                   title = fname, file = file)      # nocov
        }
    })
    suppressWarnings(raster::plotRGB (my_map))  ## #33
    grDevices::graphics.off ()

    invisible (file)
}

map_to_png <- function (my_map, file)
{
    file <- paste0 (tools::file_path_sans_ext (file), ".png")

    ex <- attributes (raster::extent (my_map))
    bb_comment <- paste0 ("EX", ex$xmin, "+", ex$ymin, "+",
                       ex$xmax, "+", ex$ymax)

    aspect <- (ex$ymax - ex$ymin) / (ex$xmax - ex$xmin)
    w <- h <- 480 * 4
    if (aspect < 1) {
        h <- 480 * 4 * aspect
    } else {
        w <- 480 * 4 / aspect       # nocov
    }

    grDevices::png (file = file, width = w, height = h, units = "px")
   suppressWarnings(raster::plotRGB (my_map))  ## #33
    grDevices::graphics.off ()

    # Then read in png, attach comment containing bbox, and re-save
    img <- magick::image_read (file)
    magick::image_write (img, path = file, comment = bb_comment)

    invisible (file)
}

convert_bbox <- function (bbox)
{
    if (is.character (bbox))
    {
        requireNamespace ("osmdata")        # nocov
        bbox <- osmdata::getbb (bbox)       # nocov
    }

    if (!is.matrix (bbox))
    {
        if (length (bbox) != 4)
            stop ("bbox must have four elements")
        bbox <- matrix (bbox, nrow = 2)
    }
    return (bbox)
}

slippy_bbox <- function (bbox)
{
    pxy <- matrix (rowMeans (bbox), nrow = 1)
    idx <- c (1, 1, 3, 3, 1,
              2, 4, 4, 2, 2)
    xy <- matrix (as.vector (bbox) [idx], ncol = 2L)

    afun <- function (aa)
        stats::approx (seq_along (aa), aa, n = 180L)$y
    srcproj <- .lonlat() #"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    crs <- .sph_merc() # "+proj=merc +a=6378137 +b=6378137"
    ex <- cbind (afun (xy [, 1L]), afun (xy [, 2L])) %>%
        reproj::reproj (target = crs, source = srcproj)
    ex <- raster::extent (ex [, 1:2])
    buffer <- c(raster::xmax(ex) - raster::xmin(ex),
                raster::ymax(ex) - raster::ymin(ex)) / 2

    loc <- slippymath::lonlat_to_merc (pxy)
    xp <- buffer[1] ## buffer is meant to be from a central point, so a radius
    yp <- buffer[2]
    ## xmin, ymin
    ## xmax, ymax
    bb_points <- matrix (c (loc [1, 1] - xp,
                            loc [1, 2] - yp,
                            loc [1, 1] + xp,
                            loc [1, 2] + yp), 2, 2, byrow = TRUE)

    ## convert bb_points back to lonlat
    bb_points_lonlat <- slippymath::merc_to_lonlat (bb_points)

    tile_bbox <- c(xmin = bb_points_lonlat [1, 1],
                   ymin = bb_points_lonlat [1, 2],
                   xmax = bb_points_lonlat [2, 1],
                   ymax = bb_points_lonlat [2, 2])

    list(tile_bbox = tile_bbox, user_points = bb_points)
}

url_to_cache <- function (x)
{
    cache <- file.path (tempdir (), ".ceramic")
    if (!fs::dir_exists (cache)) fs::dir_create (cache)
    base_filepath <- file.path (cache, gsub ("^//", "",
                                             gsub ("^https\\:", "",
                                                   gsub("^https\\:", "", x))))
    unlist (lapply (strsplit (base_filepath, "\\?"), "[", 1L))
}

# nocov start
down_loader <- function (x, query_string)
{
    purrr::pmap (x$tiles,
                function (x, y, zoom)
                {
                    api_query <- glue::glue (query_string)

                    outfile <- url_to_cache (api_query)

                    if (!file.exists (outfile) ||
                        fs::file_info (outfile)$size < 101)
                    {
                        cachedir <- fs::path_dir (outfile)

                        if (!fs::dir_exists (cachedir))
                            dir.create (cachedir, recursive = TRUE)

                        zup <- curl::curl_download (url = api_query,
                                                    outfile) # nolint
                    }
                    outfile
                },
                zoom = x$zoom)
}

get_tiles <- function (bbox_pair, max_tiles = 16L, style)
{
    bb_points <- bbox_pair$user_points

    tile_grid <- slippymath::bbox_to_tile_grid (bbox_pair$tile_bbox,
                                                max_tiles = max_tiles)
    # Maxbox style API migration:
    styles <- c ("mapbox.light", "mapbox.streets", "mapbox.outdoors")
    style <- grep (style, styles)
    style <- c ("light-v10", "streets-v11", "outdoors-v11") [style]

    # format <- "jpg" # new API is strict png only
    #baseurl <- "https://api.mapbox.com/v4" # old API
    baseurl <- "https://api.mapbox.com/styles/v1/mapbox"
    mapbox_token <- get_mapbox_token ()
    query_string <- paste0 (sprintf ("%s/%s/tiles/{zoom}/{x}/{y}",
                                     baseurl, style),
                           "?access_token=", mapbox_token)

    files <- unlist (down_loader (tile_grid, query_string))
    bad <- file.info(files)$size < 35
    if (all(bad)) {
        mess <- paste(files, collapse = "\n")
        stop(sprintf("no sensible tiles found, check cache?\n%s", mess))
    }
    user_ex <- raster::extent (as.vector (bb_points))
    list (files = files[!bad], tiles = tile_grid, extent = user_ex)
}
# nocov end


is_jpeg <- function (x)
{
  if (!file.exists (x [1]))
      return (FALSE)                                # nocov
  if (file.info (x [1])$size <= 11L)
      return (FALSE)                                # nocov
  rawb <- readBin (x[1], "raw", n = 11L)
  all (rawb[1:2] == as.raw (c (0xff, 0xd8))) &&
      rawToChar (rawb [7:11]) == "JFIF"
}

is_png <- function (x)
{
  #"89 50 4e 47 0d 0a 1a 0a"
  if (!file.exists (x[1]))
      return (FALSE)                                # nocov
  if (file.info (x [1])$size <= 8L)
      return (FALSE)                                # nocov
  rawb <- readBin (x[1], "raw", n = 8L)
  all (rawb == as.raw (c (0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)))
}

# nocov start
is_pdf <- function (x)
{
  #"25 50 44 46 2d
  if (!file.exists (x[1]))
      return (FALSE)
  if (file.info (x [1])$size <= 8L)
      return (FALSE)
  rawb <- readBin (x[1], "raw", n = 5L)
  all (rawb == as.raw (c (0x25, 0x50, 0x44, 0x46, 0x2d)))
}
# nocov end

raster_brick <- function (x) {
  out <- NULL
  if (is_jpeg (x)) {
    requireNamespace ("jpeg")
    out <- jpeg::readJPEG (x) # nocov
  } else if (is_png(x))
    out <- png::readPNG (x)
  else
    stop ("Unrecognised format; must be jpg or png") # nocov

  if (is.null (out))
      stop (sprintf ("cannot read %s", x)) # nocov
  out <- out * 255
  mode (out) <- "integer"
  ## in case it's greyscale ...
  if (length (dim (out)) == 2L)
      out <- array (out, c (dim (out), 1L))
  raster::setExtent (raster::brick (out),
                     raster::extent (0, nrow (out), 0, ncol (out)))
}

spherical_mercator <- function (provider = "mapbox")
{
    #maxextent is the bounds between [-180, 180] and [-85.0511, 85.0511]
    res <- tibble::tibble (provider = provider,
                           maxextent = 20037508.342789244,
                           A = 6378137.0, B = 6378137.0,
                           crs = .sph_merc())  ## hardcoded, not using the A,B
                           #crs = glue::glue("+proj=merc +a={A} +b={A}"))
    res [, res$provider == provider]
}

# nocov start
mercator_tile_extent <- function (tile_x, tile_y, zoom, tile_size = 256)
{
  params <- spherical_mercator (provider = "mapbox")
  params <- params [1, ]  ## FIXME: param query should provide a unique set
  maxextent <- params$maxextent
  z0_size <- (maxextent * 2)
  xlim <- -maxextent + (tile_x + c(0, 1)) * (z0_size / (2 ^ zoom))
  ylim <- range (maxextent - (tile_y + c (0, 1) - 0) * (z0_size / (2 ^ zoom)))
  stats::setNames (c (xlim, ylim), c ("xmin", "xmax", "ymin", "ymax"))
}

raster_readAll <- function (x)
{
  if (!raster::hasValues (x))
      x <- raster::readAll (x)
  x
}

fast_merge <- function (x)
{
  ## about 3 times faster than reduce(, merge)
  out <- purrr::map (x, raster::extent) %>%
      purrr::reduce (raster::union) %>%
      raster::raster (crs = raster::projection (x [[1]]))
  raster::res (out) <- raster::res (x [[1]])
  cells <- unlist (purrr::map (x, ~raster::cellsFromExtent (out, .x)))
  vals <- do.call (rbind, purrr::map (x, ~raster::values (raster_readAll (.x))))
  raster::setValues (raster::brick (out, out, out), vals [order (cells), ])
}
# nocov end
