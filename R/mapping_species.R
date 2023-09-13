#' Mapping species
#'
#' @param species
#'
#' @return an interactive map of species
#' @importFrom soilDB fetchVegdata
#' @importFrom RColorBrewer brewer.pal
#' @export
#'
#' @examples
#'#leave blank for now
#'
mapping_species <- function(veg_df, species){

  species_location <- veg_df |> dplyr::filter(plantsciname %in%
                                                stringr::str_subset(veg_df$plantsciname,
                                                                    paste(species, collapse = "|"))) |>
    dplyr::select(vegplotid, plantsciname, utmzone, utmeasting, utmnorthing) |>
      sf::st_as_sf(coords = c('utmeasting', 'utmnorthing'), crs = sf::st_crs(32611))


  species_location_split <-  split(species_location, species_location$plantsciname)

  my_colors <- RColorBrewer::brewer.pal(n = length(species_location_split), name = 'Set1')

  mapview::mapView(species_location_split, col.regions = my_colors, verbose = TRUE)

}
