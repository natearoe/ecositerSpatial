#' PRISM annual normals for ecosite
#'
#' @param ecosite ecosite id code
#' @param prism_dir directory to store PRISM data
#'
#' @return dataframe of summary stats
#' @export
#'
#' @examples
#' ecositerSpatial::prism_normals(ecosite = "F022AB100CA", prism_dir = "C:/Users/Nathan.Roe/Documents/PRISM_R/")
prism_annual_normals <- function(ecosite, prism_dir){
  # set directory where prism data is/will be located
  my_prism_dir <- prism_dir # used in multiple locations, so this is master assignment
  prism::prism_set_dl_dir(path = prism_dir)

  # define ecosite of interest
  my_ecosite <- ecosite # master assignment

  # pull SDA data for component percentage info
  comp_pct <- soilDB::SDA_query(paste("SELECT
                   mu.mukey, c.comppct_r, coeco.ecoclassid
                   FROM mapunit AS mu
                   INNER JOIN component AS c ON mu.mukey = c.mukey
                   INNER JOIN coecoclass AS coeco ON c.cokey = coeco.cokey
                   WHERE coeco.ecoclassid =",
                                      paste0("'", my_ecosite, "'"))) |>
    dplyr::group_by(mukey) |> dplyr::summarise(comp_pct = sum(comppct_r, na.rm = TRUE))

  # prism properties, names, unites
  clim_vars <- c("ppt", "tmean", "tmin",
                 "tmax", "tdmean", "vpdmin",
                 "vpdmax")

  clim_desc <- c("Total precipitation",
                 "Mean Temperature",
                 "Minimum Temperature",
                 "Maximum Temperature",
                 "Mean Dew Point Temperature",
                 "Minimum Vapur Pressure Deficit",
                 "Maximum Vapor Pressure Deficit"
  )

  clim_units <- c("Millimeters",
                  "Degrees Celsius",
                  "Degrees Celsius",
                  "Degrees Celsius",
                  "Degrees Celsius",
                  "hPa - hectopascals)",
                  "hPa - hectopascals)")



  # pull spatial data for ecosite
  eco_spatial <- soilDB::fetchSDA_spatial(my_ecosite, by.col="ecoclassid") |>
    terra::vect()
  eco_spatial$ID <- 1:nrow(eco_spatial)
  eco_spatial$ha <- terra::expanse(eco_spatial, unit = "ha")

  # get_SDA_coecoclass(method = "All",
  #                    WHERE = )

  # download prism data
  for(i in clim_vars){
    full_p <- file.path(my_prism_dir, i)
    dir.create(full_p, showWarnings = FALSE)
    prism::prism_set_dl_dir(full_p)
    prism::get_prism_normals(type = i,
                      resolution = "800m",
                      annual = TRUE)
  }

  value_names <- sapply(clim_vars, FUN = function(x){
    dir(paste0(my_prism_dir, x), )
  })
  value_names <- value_names[!grepl("\\.zip$", value_names)]

  # assign percentiles and percentile names
  my_probs <-  c(0, 0.05, .2, .8, .95, 1)
  probs_names <- c("min.", "5%", "20%", "80%", "95%", "max.")

  rast_extract <- lapply(seq_along(clim_vars), FUN = function(x){
    my_bil <- list.files(paste0(my_prism_dir, clim_vars[x]),
                         full.names = TRUE,
                         recursive = TRUE,
                         pattern = ".bil$")
    my_zone <- terra::rast(my_bil) |>
      terra::project(terra::crs(eco_spatial)) |>
      terra::extract(eco_spatial, touches = TRUE)
    my_zone |> dplyr::group_by(ID) |>
      dplyr::summarise(mean := mean(!!dplyr::sym(value_names[x]))) |>
      dplyr::left_join(as.data.frame(eco_spatial) |>
                         dplyr::select(ID, mukey, ha) |>
                         unique()) |>
      dplyr::left_join(comp_pct) |>
      dplyr::mutate(weight = ha * comp_pct * 0.01) |>
      dplyr::ungroup() |>
      dplyr::reframe(!!dplyr::sym(paste0(clim_desc[x], " (",
                                        clim_units[x], ")")) := Hmisc::wtd.quantile(mean, weights = weight,
                                                                               probs = my_probs)) |> t() |>
      as.data.frame() |> dplyr::rename_with(~ probs_names)

  })

  dplyr::bind_rows(rast_extract)
}
