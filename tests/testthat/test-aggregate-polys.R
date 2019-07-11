context("ms_aggregate_polys")

test_that ("aggregate", {
               geometry <- list (sf::st_point (cbind (0, 0)),
                                 sf::st_point (cbind (0, 1)),
                                 sf::st_point (cbind (1, 0)))
               pts <- sf::st_sf (a = 1:3, geometry = geometry)
               overlapping_polys <- sf::st_buffer (pts, 0.75)

               expect_silent (x <- ms_aggregate_polys (overlapping_polys))
               expect_is (x, "sf")
               expect_is (x$geometry, "sfc_POLYGON")
               expect_equal (nrow (x), 3)
               expect_identical (1:3, x$n)
})
