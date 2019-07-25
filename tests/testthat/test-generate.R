context("generate")

test_that ("errors", {
               expect_error (ms_generate_map (),
                             paste0 ("Please provide a 'mapname' \\(with ",
                                     "optional path\\) for the maps"))
})

test_that("generate", {
              #loc <- rbind (c (-96.12923, -96.01011),
              #              c (41.26145, 41.32220))
              #x <- ms_generate_map (loc, max_tiles = 1L, mapname = "omaha")
              #saveRDS (x, "tests/x.Rds")
              expect_silent (x <- readRDS ("../x.Rds"))
              expect_error (x <- ms_generate_map (raster_brick = x),
                            paste0 ("Please provide a 'mapname' ",
                                    "\\(with optional path\\)"))
              mapname <- file.path (tempdir (), "map")
              expect_message (x2 <- ms_generate_map (mapname = mapname,
                                                     raster_brick = x),
                              "Successfully generated")
              expect_true (identical (x, x2))
})

# ------ internal functions -----

# non-ideal, but the actual tile extraction can't be efficiently tested, so the
# remainder are tested here via calls to internal functions

test_that("convert bbox", {
              expect_error (bbc <- convert_bbox (1:5),
                            "bbox must have four elements")
              expect_silent (bbc <- convert_bbox (1:4))
              expect_is (bbc, "matrix")
              expect_equal (nrow (bbc), 2)
              expect_equal (ncol (bbc), 2)
})

test_that("slippy bbox", {
              bb <- convert_bbox (1:4)
              expect_silent (s <- slippy_bbox (bb))
              expect_is (s, "list")
              expect_identical (names (s), c ("tile_bbox", "user_points"))
              expect_is (s$tile_bbox, "numeric")
              expect_equal (length (s$tile_bbox), 4)
              expect_is (s$user_points, "matrix")
})

test_that("url_to_cache", {
              query_string <- paste0 ("https://api.mapbox.com/v4/mapbox.light/",
                                      "{zoom}/{x}/{y}.jpg?access_token=",
                                      "123456789")
              expect_silent (outfile <- url_to_cache (query_string))
              expect_is (outfile, "character")
              expect_false (file.exists (outfile))
              expect_false (dir.exists (outfile))
})

test_that("raster_brick", {
              f <- system.file ("extdata", "omaha.png", package = "mapscanner")
              expect_silent (rb <- raster_brick (f))
              expect_is (rb, "RasterBrick")
})

test_that ("spherical_mercator", {
               expect_silent (x <- spherical_mercator ())
               expect_is (x, "tbl")
               expect_equal (nrow (x), 1)
               expect_equal (ncol (x), 5)
               expect_identical (names (x), c ("provider", "MAXEXTENT", "A",
                                               "B", "crs"))
})
