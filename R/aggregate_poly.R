#' Aggregate overlapping polygons
#'
#' Aggregate a 'Simple Features' (\pkg{sf}) `data.frame` of `n` polygons to a series
#' of aggregate polygons representing up to `n` distinct levels. No attributes
#' data of input data is considered.
#'
#' @param px An \pkg{sf} `data.frame` of input (multi)polygons, assuming to be
#' overlapping.
#' @param ... unused
#' @return An equivalent \pkg{sf} `data.frame` of polygons in which the column
#' `n` represents the aggregate number covering the specified polygonal area.
#'
#' @note This function is intended for cases in which `mapscanner` is used in
#' social surveys aimed at defining specific areas. The aggregate results from
#' `n` polygon definitions can then be (row-)joined together into a single
#' \pkg{sf} `data.frame` and submitted to this function to obtain a vector form
#' of a 'heat map' identifying the area of greatest overlap. Particular contours
#' of this heatmap can be extracted by sub-setting or filtering the resultant
#' `data.frame` by desired values of `n`.
#'
#' @export
#' @examples
#' pts <- sf::st_sf (a = 1:3,
#'                   geometry = sf::st_sfc (list (sf::st_point (cbind (0, 0)),
#'                                                sf::st_point (cbind (0, 1)),
#'                                                sf::st_point (cbind (1, 0)))))
#' overlapping_polys <- sf::st_buffer (pts, 0.75)
#'
#' ## decompose and count space-filling from overlapping polygons
#' x <- ms_aggregate_poly (overlapping_polys)
#' plot (x)
#' #library (ggplot2)
#' #ggplot (x) + geom_sf () + facet_wrap (~n)
ms_aggregate_poly <- function (px, ...)
{
    # no visible binding notes:
    .data <- feature <- n <- . <- NULL

    ## convert to unioned lines, so all edges in one geometry
    mesh <- sf::st_cast (px, "MULTILINESTRING") %>%
        sf::st_union () %>%
        sfdct::ct_triangulate () %>%
        sf::st_cast ()
    ## centroids of the primitives is the mapping of primitive back to original feature
    mesh_centroids <- sf::st_centroid (sf::st_cast (mesh))


    ## flesh out primitives to feature index
    triangle <- unlist (sf::st_intersects (px, mesh_centroids))
    feat_index <- tibble::tibble (triangle = triangle) %>%
        dplyr::mutate (feature =
                lapply (sf::st_intersects (mesh_centroids [.data$triangle], px),
                        function (.x) tibble::tibble (feature = .x)))

    #a <- do.call(rbind, intscts %>%
    #             tidyr::unnest() %>%
    #             group_by(triangle, feature) %>%
    #             mutate(n = n()) %>%
    #             ungroup() %>%
    #             group_by(n) %>%
    #             split(.$n) %>%
    #             purrr::map(~st_sf(geometry = st_union(mesh[.x$triangle]), n = .x$n[1])))

    ## unnest
    triangle <- rep (feat_index$triangle, unlist (lapply (feat_index$feature, nrow)))
    feat_index <- tibble::tibble (triangle = triangle,
                                  feature = do.call (rbind,
                                                     feat_index$feature)$feature)
    counts <- feat_index %>%
        dplyr::group_by (triangle, feature) %>%
        dplyr::mutate (n = dplyr::n ()) %>%
        dplyr::ungroup () %>%
        dplyr::group_by (.data$n)

    ## now collate and build output
    levcounts <- unique (counts$n)
    outlist <- vector ("list", length (levcounts))
    for (i in seq_along (levcounts)) {
        i_feature <- counts %>% dplyr::filter (n >= levcounts [i])

        outlist [[i]] <- sf::st_union (mesh [i_feature$triangle]) %>%
            sf::st_sf (geometry = ., n = levcounts [i])
    }
    do.call (rbind, outlist) #%>% dplyr::arrange(dplyr::desc(.data$n))
}
