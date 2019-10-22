context("file manipulation")

skip_on_cran ()

test_that("file manipulation", {
              f_orig <- system.file ("extdata", "omaha.png",
                                     package = "mapscanner")
              f_modified <- system.file ("extdata", "omaha-polygons.png",
                                         package = "mapscanner")

              requireNamespace ("mmand")
              f1 <- list.files (tempdir ())
              png (file.path (tempdir (), "junk.png"))
              expect_silent (ms_rotate_map (f_orig, f_modified))
              graphics.off ()
              f2 <- list.files (tempdir ())
              f2 <- f2 [!f2 %in% f1]
              expect_true ("junk.png" %in% f2)
              expect_true ("test.png" %in% f2) # default name of rotated image
})
