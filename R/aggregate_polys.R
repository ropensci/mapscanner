#' Aggregate disparate polygons
#'
#' Planar partition from disparate polygon inputs. Overlaps aggregate to `n`.
#'
#' Input is a single simple features polygon data frame. No attribute data is
#' considered.
#' @param px input polygons (assumed overlapping poly/mpolys in sf_df)
#' @param ... unused
#' @export
#' @importFrom rlang .data
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
ms_aggregate_polys <- function (px, ...)
{
    tri_map <- triangulate_map_sf (px)

    n_types <- max (lengths (split (tri_map$index$path_,
                                    tri_map$index$triangle_idx)))
    ## note that these are now overlapping polygons, with a record of n-pieces,
    ## so we order in increasing n for the plot, and we don't need to build n ==
    ## 1 from fragments because that's the union of of the input
    sf::st_cast (rbind (sf::st_sf (n = 1, geometry = sf::st_union (px)),
                        do.call (rbind, lapply (seq_len (n_types)[-1],
                                                function (ni) {
                            sf_df (n_intersections (tri_map, ni), n = ni)
                                                }
                                                ))), "MULTIPOLYGON")

}



p_paste <- function (x, paster = function (...) paste (..., sep = "-"))
{
    do.call (paster,
             x [intersect (names (x), c ("object", "subobject", "path"))])
}


sf_df <- function (x, n) {
    sf::st_sf (n = n, geometry = sf::st_union (x)) %>%
        sf::st_cast ("MULTIPOLYGON")
}


# combination of path <- silicate::PATH(sfall);
# RTri <- pfft::edge_RTriangle(path)
triangulate_map_sf <- function (x, ...)
{
    requireNamespace ("dplyr")
    requireNamespace ("gibble")

    ## we need all coordinates in order, and their normalized form
    ## (unique in x, y)
    coord0 <- tibble::as_tibble (sf::st_coordinates (x) [, 1:2]) %>%
        stats::setNames (c ("x_", "y_"))
    udata <- unjoin::unjoin (coord0, .data$x_, .data$y_, key_col = "vertex_")
    udata [["vertex_"]]$row <- seq_len (nrow (udata[["vertex_"]]))

    ## the number of coordinates in paths, in order
    gmap <- gibble::gibble (x)
    gmap [["path"]] <- seq_len (nrow (gmap))

    ## map between coordinates as instances vertices, and object (feature), path
    path <- as.integer (factor (rep (p_paste (gmap), gmap$nrow)))
    instances <- dplyr::mutate (udata$data, path = path,
                                object = rep (gmap$object, gmap$nrow),
                                coord = dplyr::row_number ())
    if (length (unique (instances$path)) == nrow (instances))
    {

        print ("Unknown error in map triangulation") # nocov
    } else
    {
        ## convert to edge-based rather than path-based
        segs0 <-   dplyr::mutate (instances[c ("path", "coord", "object")],
                                  .cx0 = .data$coord, .cx1 = .data$coord + 1L)
        segs <- dplyr::group_by (segs0, .data$path) %>%
            dplyr::slice (-dplyr::n ()) %>%
            dplyr::ungroup () %>%
            dplyr::transmute (.data$.cx0, .data$.cx1, .data$path,
                              .data$object)
            segs[[".vx0"]] <- instances$vertex_[match (segs$.cx0,
                                                       instances$coord)]
            segs[[".vx1"]] <- instances$vertex_[match (segs$.cx1,
                                                       instances$coord)]

    }

    ## build graph and triangulate with edge constraints
    P <- as.matrix (dplyr::arrange (udata [["vertex_"]],
                                    .data$vertex_) [c ("x_", "y_")])
    ps <- RTriangle::pslg (P = P,
                           S = as.matrix (segs[c (".vx0", ".vx1")]))
    RTri <- RTriangle::triangulate (ps)

    ## RTri is output of triangulate_sf
    ## now need map <- pfft::path_triangle_map (path, RTri)
    ## find mapping between triangle and feature by centroid lookup (we only
    ## look in relevant bbox for each feature)
    tri_split <- split (RTri [["P"]] [t (RTri[["T"]]), ],
                        rep (seq (nrow (RTri$T)), each = 3))
    centroids <- matrix (unlist (lapply (tri_split, .colMeans, 3, 2)),
                         ncol = 2, byrow = TRUE)
    ex <- purrr::map_dfr (split (instances["coord"],
                                 instances$path) [unique (instances$path)],
                         ~coord0[.x$coord, ] %>%
                             dplyr::summarize (xmn = min (x_),
                                               xmx = max (x_),
                                               ymn = min (y_),
                                               ymx = max (y_)))
    ex$path_ <- seq_len (nrow (ex))
    gm <- gibble::gibble (x)
    pipmap <- purrr::transpose (ex) %>%
        purrr::map (~(centroids[,1] >= .x[["xmn"]] & # nolint
                      centroids[, 1] <= .x[["xmx"]] &
                      centroids[, 2] >= .x[["ymn"]] &
                      centroids[, 2] <= .x[["ymx"]]))
    pipmap <- pipmap[ex$path_]
    pipmap <- stats::setNames (pipmap, as.character (seq_along (pipmap)))

    ## n points in each path
    len <- purrr::map_int (pipmap, sum)

    ## coordinates of each path (for coming lookup)
    lc <- split (coord0, rep (seq_len (nrow (gm)),
                            gm$nrow))
    pip <- pipmap

    ## loop over path and only to pip lookup for triangle centroid inside this
    ## features's bbox
    for (i in seq_along (pipmap))
    {
        if (len[i] > 0) {
            centr_in <- list (x = centroids[pipmap[[i]], 1],
                            y = centroids[pipmap[[i]], 2])
            pip_in <- list (x = lc[[i]][["x_"]], y = lc[[i]][["y_"]])
            pip_out <- polyclip::pointinpolygon (centr_in, pip_in)
            pip[[i]] [pipmap[[i]]] <- abs (pip_out) > 0L
        }
    }

    ## collate indexes and return a) input layers, b) triangles, c) mapping
    ## between feature and path and input layer d) triangle index to path
    ix <- lapply (pip, which)
    gm$path_ <- ex$path_
    list (input = list (x),
          primitives = RTri,
          geometry_map = gm %>%
              dplyr::transmute (.data$subobject,
                                object_ = .data$object,
                                ncoords_ = .data$nrow,
                                path = .data$path_,
                                layer = 1),
          index = tibble::tibble (path_ = as.integer (rep (names (ix),
                                                           lengths (ix))),
                                  triangle_idx = unlist (ix)))
}

n_intersections <- function (x, n = 2, ...)
{
        triangles <- x$index %>%
            dplyr::group_by (.data$triangle_idx) %>%
            dplyr::mutate (nn = dplyr::n ()) %>%
            dplyr::ungroup () %>%
            dplyr::filter (.data$nn >= n) %>%
            dplyr::transmute (path = .data$path_, .data$triangle_idx)
        gmap <- x$geometry_map %>%
            dplyr::select (.data$object_, .data$layer, .data$path)
        ## every unique triangle keeps a record of which path, object, layer
        ## (a bit of redundancy until we get a single path/object index or ...)
            ## path joins us to layer + object
        idx <- triangles %>% dplyr::inner_join(gmap, "path")


        ## now build each triangle
        P <- x$primitives$P
        TR <- x$primitives$T
        g <- purrr::map (idx$triangle_idx,
                    ~sf::st_polygon (list (P [TR [.x, ] [c (1, 2, 3, 1)], ])))
        sf::st_sf (idx = idx$triangle_idx, geometry = sf::st_sfc (g))
    }
