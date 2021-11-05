#' Aggregate disparate polygons
#'
#' Planar partition from disparate polygon inputs. Overlaps aggregate to `n`.
#'
#' Input is a single simple features polygon data frame. No attribute data is
#' considered.
#' @param p input (multi-)polygons (assumed to be overlapping)
#' @export
#' @examples
#' g <- sf::st_sfc(list(sf::st_point(cbind(0, 0)),
#'                      sf::st_point(cbind(0, 1)),
#'                      sf::st_point(cbind(1, 0))))
#' pts <- sf::st_sf(a = 1:3,  geometry = g)
#' overlapping_polys <- sf::st_buffer(pts, 0.75)
#'
#' ## decompose and count space-filling from overlapping polygons
#' x <- ms_aggregate_polys(overlapping_polys)
#' plot(x)
#' # library(ggplot2)
#' # ggplot(x) + geom_sf() + facet_wrap(~n)
#'
#' library(sf)
#' set.seed(6)
#' pts <- expand.grid (x = 1:8, y = 1:10) %>% st_as_sf (coords = c("x", "y"))
#' xsf <- sf::st_buffer (pts, runif (nrow (pts), 0.2, 1.5))
#' # out <- ms_aggregate_polys (xsf)
ms_aggregate_polys <- function (p) {

    p <- sf::st_intersection (p)

    n <- seq (max (p$n.overlaps))
    g <- lapply (n, function (i) {
                     gi <- p$geometry [p$n.overlaps >= i]
                     out <- sf::st_union (gi)
                     if (sf::st_geometry_type (out) == "GEOMETRYCOLLECTION") {
                         out <- sf::st_collection_extract (out)
                     }
                     return (out)
    })

    sf::st_sf (n = n, geometry = do.call (c, g))
}
