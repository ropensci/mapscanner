# most of this pilfered and otherwise mildly adapted from code by @mdsumner from
# hypertidy/ceramic

#' ms_get_map
#'
#' Get a map image for a specified area or bounding box
#'
#' @param bbox Either a string specifying the location, or a numeric bounding
#' box as a single vector of (xmin, ymin, xmax, ymax), or a 2-by-2 matrix with
#' columns of (min, max) and rows of (x, y), respectively.
#' @param max_tiles Maximum number of tiles to use to create map
#' @return A map
#' @export
ms_get_map <- function (bbox, max_tiles = 16L)
{
    bbox <- convert_bbox (bbox)
    bbox_pair <- slippy_bbox (bbox)
    tiles <- get_tiles (bbox_pair, max_tiles = max_tiles)
    tgrid <- tiles$tiles
    br <- lapply (tiles$files, raster_brick)

    for (i in seq_along (br)) {
        br [[i]] <- raster::setExtent (br [[i]],
                                       mercator_tile_extent (tgrid$tiles$x [i],
                                                             tgrid$tiles$y [i],
                                                             zoom = tgrid$zoom))
    }

    out <- fast_merge (br)
    raster::projection (out) <- "+proj=merc +a=6378137 +b=6378137"
    raster::crop (out, tiles$extent , snap = "out")
}

#' print map to a pdf file
#'
#' @param my_map A map produced with \link{ms_get_map}.
#' @param file Name of pdf file to print to (extension with be automatically
#' added).
#' @return Nothing
#' @export
ms_map_to_pdf <- function (my_map, file)
{
    file <- paste0 (tools::file_path_sans_ext (file), ".pdf")

    ex <- attributes (raster::extent (my_map))
    aspect <- (ex$ymax - ex$ymin) / (ex$xmax - ex$xmin)
    A4L <- 11.7 # A4 paper is 8.3-by-11.7
    A4H <- 8.3
    # the following issue warnings about mode(onefile)
    suppressWarnings ({
        if (aspect < 1) {
            grDevices::pdf (my_map, width = A4L, paper = "a4r",
                            colormodel = "gray", file = file)
        } else {
            grDevices::pdf (my_map, height = A4L, paper = "a4",
                            colormodel = "gray", file = file)
        }
    })
    raster::plotRGB (my_map)
    grDevices::graphics.off ()
}

convert_bbox <- function (bbox)
{
    if (is.character (bbox))
    {
        requireNamespace (osmdata)
        bbox <- osmdata::getbb (bbox)
    }

    if (!is.matrix (bbox))
    {
        bbox <- matrix (bbox, nrow = 2)
        if (ncol (bbox) != 2)
            stop ("bbox must have four elements")
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
    srcproj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    crs <- "+proj=merc +a=6378137 +b=6378137"
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

                        zup <- curl::curl_download (url = api_query, outfile)
                    }
                    outfile
                },
                zoom = x$zoom)
}

get_tiles <- function (bbox_pair, max_tiles = 16L)
{
    bb_points <- bbox_pair$user_points

    tile_grid <- slippymath::bbox_to_tile_grid (bbox_pair$tile_bbox,
                                                max_tiles = max_tiles)
    zoom <- tile_grid$zoom

    type <- "mapbox.light"
    provider <- "mapbox"
    format <- "jpg"
    #query_string <- mapbox_string(type = type, format = format)
    baseurl <- "https://api.mapbox.com/v4"
    mapbox_token <- Sys.getenv ("MAPBOX_TOKEN")
    query_string <- paste0(sprintf("%s/%s/{zoom}/{x}/{y}.%s", baseurl, type, format),
                           "?access_token=", mapbox_token)

    files <- unlist (down_loader (tile_grid, query_string))
    bad <- file.info(files)$size < 35
    if (all(bad)) {
        mess <- paste(files, collapse = "\n")
        stop(sprintf("no sensible tiles found, check cache?\n%s", mess))
    }
    user_x <- NULL
    user_ex <- raster::extent(as.vector(bb_points))
    list (files = files[!bad], tiles = tile_grid, extent = user_ex)
}


is_jpeg <- function (x)
{
  if (!file.exists (x [1]))
      return (FALSE)
  if (file.info (x [1])$size <= 11L)
      return (FALSE)
  rawb <- readBin (x[1], "raw", n = 11L)
  all (rawb[1:2] == as.raw (c (0xff, 0xd8))) && rawToChar (rawb [7:11]) == "JFIF"
}

is_png <- function (x)
{
  #"89 50 4e 47 0d 0a 1a 0a"
  if (!file.exists (x[1]))
      return (FALSE)
  if (file.info (x [1])$size <= 8L)
      return (FALSE)
  rawb <- readBin (x[1], "raw", n = 8L)
  all (rawb == as.raw (c (0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)))
}

raster_brick <- function (x) {
  out <- NULL
  if (is_jpeg (x))
    out <- jpeg::readJPEG (x)
  else if (is_png(x))
    out <- png::readPNG (x)
  else
    stop ("Unrecognised format; must be jpg or png")

  if (is.null (out))
      stop (sprintf ("cannot read %s", x))
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
    #MAXEXTENT is the bounds between [-180, 180] and [-85.0511, 85.0511]
    tibble::tibble (provider = provider,
                    MAXEXTENT = 20037508.342789244,
                    A = 6378137.0, B = 6378137.0,
                    crs = glue::glue("+proj=merc +a={A} +b={A}")) %>%
        dplyr::filter (provider == provider)
}

mercator_tile_extent <- function (tile_x, tile_y, zoom, tile_size = 256)
{
  params <- spherical_mercator (provider = "mapbox")
  params <- params [1, ]  ## FIXME: param query should provide a unique set, this is WIP
  MAXEXTENT <- params$MAXEXTENT
  A <- params$A
  z0_size <- (MAXEXTENT * 2)
  xlim <- -MAXEXTENT + (tile_x + c(0, 1)) * (z0_size / (2 ^ zoom))
  ylim <- range (MAXEXTENT - (tile_y + c (0, 1) - 0) * (z0_size / (2 ^ zoom)))
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
