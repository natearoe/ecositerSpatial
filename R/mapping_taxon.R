#' Mapping taxon
#'
#' @param taxon
#'
#' @return an interactive map of taxon
#' @importFrom RColorBrewer brewer.pal
#' @export
#'
#' @examples
#'#leave blank for now
#'
mapping_taxon <- function(veg_df, taxon){

  taxon_location <- veg_df |> dplyr::filter(plantsciname %in%
                                                stringr::str_subset(veg_df$plantsciname,
                                                                    paste(taxon, collapse = "|"))) |>
    dplyr::select(vegplotid, plantsciname, utmzone, utmeasting, utmnorthing) |>
      sf::st_as_sf(coords = c('utmeasting', 'utmnorthing'), crs = sf::st_crs(32611))


  taxon_location_split <-  split(taxon_location, taxon_location$plantsciname)

  my_colors <- RColorBrewer::brewer.pal(n = length(taxon_location_split), name = 'Set1')

  mapview::mapView(taxon_location_split, col.regions = my_colors, verbose = TRUE)

}
