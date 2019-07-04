context("basic functionality")

test_that("rectify", {
              f_orig <- system.file ("extdata", "omaha.jpg",
                                     package = "mapscanner")
              expect_true (file.exists (f_orig))
              expect_true (is_jpeg (f_orig))
              f_modified <- system.file ("extdata", "omaha_drawn.jpg",
                                         package = "mapscanner")
              expect_true (file.exists (f_modified))
              expect_true (is_jpeg (f_modified))

              # have to down-scale images for testing because RNiftyReg takes
              # way too long
              f_orig2 <- file.path (tempdir (), "omaha.jpg")
              f_modified2 <- file.path (tempdir (), "omaha_drawn.jpg")
              magick::image_read (f_orig) %>%
                  magick::image_resize ("25%") %>%
                  magick::image_write (f_orig2)
              magick::image_read (f_modified) %>%
                  magick::image_resize ("25%") %>%
                  magick::image_write (f_modified2)

              expect_silent (res_p <- ms_rectify_maps (f_orig2, f_modified2,
                                                       type = "polygons"))
              expect_is (res_p, "sf")
              expect_is (res_p$geometry, "sfc_POLYGON")

              expect_silent (res_h <- ms_rectify_maps (f_orig2, f_modified2,
                                                       type = "hulls"))
              expect_is (res_h, "sf")
              expect_is (res_h$geometry, "sfc_POLYGON")
              expect_true (all (sf::st_area (res_h) > sf::st_area (res_p)))

              expect_silent (res_1 <- ms_rectify_maps (f_orig2, f_modified2,
                                                       type = "points"))
              expect_is (res_1, "sf")
              expect_is (res_1$geometry, "sfc_MULTIPOINT")
})
