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


  veg_data <- soilDB::fetchVegdata(dsn = "C:/Users/Nathan.Roe/Documents/SEKI/CA792_veg_data.sqlite", SS = FALSE)

  species_df <- veg_data$vegplotspecies |>
    dplyr::filter(plantsciname %in% stringr::str_subset(veg_data$vegplotspecies$plantsciname, paste(species, sep = "|"))) |>
    dplyr::select(vegplotid, plantsciname)

  species_location <-
    veg_data$vegplotlocation |> dplyr::filter(vegplot_id %in% species_df$vegplotid) |>
    dplyr::select(vegplot_id, utmzone, utmeasting, utmnorthing) |> unique() |>
    dplyr::right_join(species_df, multiple = "all", by = dplyr::join_by(vegplot_id == vegplotid)) |> sf::st_as_sf(coords = c('utmeasting', 'utmnorthing'),
                                                crs = sf::st_crs(32611))

  species_location_split <-  split(species_location, species_location$plantsciname)


  my_colors <- RColorBrewer::brewer.pal(n = length(species_location_split), name = 'Set1')


  mapview::mapView(species_location_split, col.regions = my_colors, verbose = TRUE)
}
