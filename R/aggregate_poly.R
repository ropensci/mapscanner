#' Aggregate disparate polygons
#'
#' Planar partition from disparate polygon inputs. Overlaps aggregate to `n`.
#'
#' Input is a single simple features polygon data frame. No attribute data is considered.
#' @param px input polygons (assumed overlapping poly/mpolys in sf_df)
#' @param ... unused
#' @export
#' @examples
#' pts <- sf::st_sf(a = 1:3,
#'     geometry = sf::st_sfc(list(sf::st_point(cbind(0, 0)), sf::st_point(cbind(0, 1)), sf::st_point(cbind(1, 0)))))
#' overlapping_polys <- sf::st_buffer(pts, 0.75)
#'
#' ## decompose and count space-filling from overlapping polygons
#' x <- ms_aggregate_poly(overlapping_polys); plot(x)
#' #library(ggplot2)
#' #ggplot(x) + geom_sf() + facet_wrap(~n)
ms_aggregate_poly <- function(px, ...) {

  ## convert to unioned lines, so all edges in one geometry
  mesh <- sf::st_cast(sfdct::ct_triangulate(sf::st_union(sf::st_cast(px, "MULTILINESTRING"))))
  ## centroids of the primitives is the mapping of primitive back to original feature
  mesh_centroids <- sf::st_centroid(sf::st_cast(mesh))


  ## flesh out primitives to feature index
  feat_index <- tibble::tibble(triangle = unlist(sf::st_intersects(px, mesh_centroids))) %>%
   dplyr::mutate(feature = lapply(sf::st_intersects(mesh_centroids[.data$triangle], px),
                            function(.x) tibble::tibble(feature = .x)))

  #a <- do.call(rbind, intscts %>% tidyr::unnest() %>% group_by(triangle, feature) %>% mutate(n = n()) %>%
  #               ungroup() %>% group_by(n) %>% split(.$n) %>%
  #               purrr::map(~st_sf(geometry = st_union(mesh[.x$triangle]), n = .x$n[1])))

  ## unnest
  feat_index <- tibble::tibble(triangle = rep(feat_index$triangle, unlist(lapply(feat_index$feature, nrow))),
                               feature = do.call(rbind, feat_index$feature)$feature)
  counts <- feat_index %>% dplyr::group_by(triangle, feature) %>% dplyr::mutate(n = dplyr::n()) %>%
    dplyr::ungroup() %>% dplyr::group_by(.data$n)

  ## now collate and build output
  levcounts <- unique(counts$n)
  outlist <- vector("list", length(levcounts))
  for (i in seq_along(levcounts)) {
    i_feature <- counts %>% dplyr::filter(n >= levcounts[i])

    outlist[[i]] <- sf::st_sf(geometry = sf::st_union(mesh[i_feature$triangle]),
                              n = levcounts[i])

  }
  do.call(rbind, outlist) #%>% dplyr::arrange(dplyr::desc(.data$n))
}
