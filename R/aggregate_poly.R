#' Aggregate disparate polygons
#'
#' Planar partition from disparate polygon inputs. Overlaps aggregate to `n`.
#'
#' Input is a single simple features polygon data frame. No attribute data is considered.
#' @param px input polygons (assumed overlapping poly/mpolys in sf_df)
#' @param ... unused
#' @export
#' @importFrom rlang .data
#' @examples
#' g <- sf::st_sfc(list(sf::st_point(cbind(0, 0)),
#'                                 sf::st_point(cbind(0, 1)),
#'                                 sf::st_point(cbind(1, 0))))
#' pts <- sf::st_sf(a = 1:3,  geometry = g)
#' overlapping_polys <- sf::st_buffer(pts, 0.75)
#'
#' ## decompose and count space-filling from overlapping polygons
#' x <- ms_aggregate_poly(overlapping_polys); plot(x)
#' #library(ggplot2)
#' #ggplot(x) + geom_sf() + facet_wrap(~n)
#'
#' library(sf)
#' set.seed(6)
#' pts <- expand.grid(x = 1:8, y = 1:10) %>% st_as_sf(coords = c("x", "y"))
#' xsf <- sf::st_buffer (pts, runif(nrow(pts), 0.2, 1.5))
#' #system.time(out <- ms_aggregate_poly(xsf))
ms_aggregate_poly <- function(px, ...) {
 tri_map <- triangulate_map_sf(px)

 n_types <- max_overlaps(tri_map)

 ## note that these are now overlapping polygons, with a record of n-pieces,
 ## so we order in increasing n for the plot, and we don't need to build n == 1 from fragments because that's
 ## the union of of the input
 rbind(sf::st_sf(n = 1, geometry = sf::st_union(x)),
       do.call(rbind, purrr::map(seq_len(n_types)[-1], ~{tri_map %>% n_intersections(.x) %>% sf_df(n = .x)})))

}



p_paste <- function (x, paster = function(...) paste(..., sep = "-"))
{
    do.call(paster, x[intersect(names(x), c("object", "subobject",
                                            "path"))])
}

max_overlaps <- function(x) {
    x$index %>%
        dplyr::group_by(.data$triangle_idx) %>%
        dplyr::mutate(nn = dplyr::n()) %>% dplyr::pull(nn) %>% max()
}

sf_df <- function(x, n) sf::st_sf(n = n, geometry = sf::st_union(x)) %>% sf::st_cast("MULTIPOLYGON")



# combination of path <- silicate::PATH(sfall); RTri <- pfft::edge_RTriangle(path)
triangulate_map_sf <- function (x, ...)
{
    coord0 <- sf::st_coordinates(x)[,1:2] %>% tibble::as_tibble() %>% dplyr::rename(x_ = .data$X, y_ = .data$Y)
    udata <- unjoin::unjoin(coord0, .data$x_, .data$y_, key_col = "vertex_")
    udata[["vertex_"]]$row <- seq_len(nrow(udata[["vertex_"]]))
    gmap <- gibble::gibble(x) %>% dplyr::mutate(path = dplyr::row_number())
    instances <- udata$data %>%
        dplyr::mutate(path = as.integer(factor(rep(p_paste(gmap), gmap$nrow))),
                      object = rep(gmap$object, gmap$nrow), coord = dplyr::row_number())
    object <- tibble::tibble(object_ = seq_len(nrow(x)))
    if (length(unique(instances$path)) == nrow(instances)) {

        print("tell Mike")
        # instances[".vx0"] <- instances["vertex_"]
        # object$topology_ <- split(instances[c(".vx0")], instances$object)
    }else {
        segs <- instances %>% dplyr::select(.data$path, .data$coord,
                                            .data$object) %>% dplyr::mutate(.cx0 = .data$coord,
                                                                            .cx1 = .data$coord + 1L) %>% dplyr::group_by(.data$path) %>%
            dplyr::slice(-dplyr::n()) %>% dplyr::ungroup() %>%
            dplyr::transmute(.data$.cx0, .data$.cx1, .data$path,
                             .data$object)
        segs[[".vx0"]] <- instances$vertex_[match(segs$.cx0,
                                                  instances$coord)]
        segs[[".vx1"]] <- instances$vertex_[match(segs$.cx1,
                                                  instances$coord)]

    }

    ps <- RTriangle::pslg(P = as.matrix(udata[["vertex_"]] %>% dplyr::arrange(vertex_) %>% dplyr::select(.data$x_, .data$y_)),
                          S = segs %>% dplyr::select(.data$.vx0, .data$.vx1) %>% as.matrix())
    RTri <- RTriangle::triangulate(ps)
    ## RTri is output of triangulate_sf
    ## now need map <- pfft::path_triangle_map(path, RTri)
    centroids <- matrix(unlist(lapply(split(RTri[["P"]][t(RTri[["T"]]), ], rep(seq(nrow(RTri$T)), each = 3)), .colMeans, 3, 2)), ncol = 2, byrow = TRUE)
    ex <- purrr::map_dfr(split(instances["coord"], instances$path)[unique(instances$path)],
                         ~coord0[.x$coord, ] %>% dplyr::summarize(xmn = min(x_), xmx = max(x_), ymn = min(y_), ymx = max(y_)))
    ex$path_ <- seq_len(nrow(ex))
    gm <- gibble::gibble(x)
    pipmap <- purrr::transpose(ex) %>% purrr::map(~(centroids[,1] >= .x[["xmn"]] & centroids[, 1] <= .x[["xmx"]] &
                                                        centroids[, 2] >= .x[["ymn"]] & centroids[, 2] <= .x[["ymx"]]))
    pipmap <- pipmap[ex$path_]
    pipmap <- stats::setNames(pipmap, as.character(seq_along(pipmap)))
    len <- purrr::map_int(pipmap, sum)
    lc <- split(coord0, rep(seq_len(nrow(gm)),
                            gm$nrow))
    pip <- pipmap
    for (i in seq_along(pipmap)) {
        if (len[i] > 0) {
            #print(i)
            #browser()
            # pip[[i]] <- abs(polyclip::pointinpolygon(list(x = centroids[, 1], y = centroids[, 2]),
            #                                                       list(x = lc[[i]][["x_"]], y = lc[[i]][["y_"]]))) > 0L
            pip[[i]][pipmap[[i]]] <- abs(polyclip::pointinpolygon(list(x = centroids[pipmap[[i]], 1], y = centroids[pipmap[[i]], 2]),
                                                                  list(x = lc[[i]][["x_"]], y = lc[[i]][["y_"]]))) > 0L

        }# else {
        #  pip[[i]][] <- FALSE
        #}
    }
    ix <- lapply(pip, which)
    gm$path_ <- ex$path_
    list(input = list(x),
         primitives = RTri,
         geometry_map = gm %>% dplyr::transmute(subobject, object_ = object, ncoords_ = nrow, path = path_, layer = 1),
         index = tibble::tibble(path_ = as.integer(rep(names(ix), lengths(ix))), triangle_idx = unlist(ix)))
}

n_intersections <-
function(x, n = 2, ...) {
    triangles <- x$index %>%
        dplyr::group_by(.data$triangle_idx) %>%
        dplyr::mutate(nn = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::filter(.data$nn >= n) %>%
        dplyr::transmute(path = .data$path_, .data$triangle_idx)
    gmap <- x$geometry_map %>%
        dplyr::select(.data$object_, .data$layer, .data$path)
    ## every unique triangle keeps a record of which path, object, layer
    ## (a bit of redundancy until we get a single path/object index or ...)
    idx <- purrr::map_df(split(triangles, triangles$triangle_idx),
                         function(piece) {
                             ## path joins us to layer + object
                             piece %>% dplyr::inner_join(gmap, "path")
                         }) %>% dplyr::group_by(.data$triangle_idx) %>% tidyr::nest()

    ## now build each triangle
    P <- x$primitives$P
    TR <- x$primitives$T
    sf::st_sf(idx = idx, geometry = sf::st_sfc(purrr::map(idx$triangle_idx, ~sf::st_polygon(list(P[TR[.x, ][c(1, 2, 3, 1)], ])))))
}
