#' Mapping taxon
#' @param veg_df vegetation dataframe, likely created using `ecositer::create_veg_df()`
#' @param taxon taxon of interest
#' @param x x coordinate (i.e., easting or longitude) column from veg_df
#' @param y y coordinate (i.e., northing or latitude) column from veg_df
#' @param EPSG the EPSG code for the coordinate reference system (CRS) of the input data, in format EPSG:36211
#'
#' @return an interactive map of taxon
#' @importFrom RColorBrewer brewer.pal
#' @export
#'
#' @examples
#' ecositerSpatial::mapping_taxon(veg_df = my_veg_df, taxon = "Pinus", x = "utmeasting", y = "utmnorthing", EPSG = "EPSG:32611")
#'
mapping_taxon <- function(veg_df, taxon, x, y, EPSG){

  if(!x %in% colnames(veg_df)){
    stop("x is not a column in dataframe")
  }

  if(!y %in% colnames(veg_df)){
    stop("y is not a column in dataframe")
  }

  tryCatch({
    crs_info <- terra::crs(EPSG)
  },
  error = function(e){
    stop(sprintf("%s is not recognized. Please provide valid EPSG in format 'EPSG:32611'.", EPSG))
  }
  )

  taxon_location <- veg_df |> dplyr::filter(plantsciname %in%
                                                stringr::str_subset(veg_df$plantsciname,
                                                                    paste(taxon, collapse = "|"))) |>
    dplyr::select(vegplotid, plantsciname, x, y) |>
      # sf::st_as_sf(coords = c('utmeasting', 'utmnorthing'), crs = sf::st_crs(32611))
      terra::vect(geom = c(x, y), crs = EPSG) |> sf::st_as_sf()


  taxon_location_split <-  split(taxon_location, taxon_location$plantsciname)

  my_colors <- RColorBrewer::brewer.pal(n = length(taxon_location_split), name = 'Set1')[1:length(taxon_location_split)]

  mapview::mapView(taxon_location_split, col.regions = my_colors, verbose = TRUE)

}
