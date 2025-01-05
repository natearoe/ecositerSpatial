#' Extract PRISM annual normals for sites
#'
#' @description
#' This function allows you to extract PRISM normals for all the sites in your dataframe
#'
#'
#' @param site_df a dataframe containing geographic data - for ecositer users, most likely a veg_df which contains eastings/northings.
#' @param prism_dir location of PRISM data (either exists - data will not be downloaded, or non-existing - data will be downloaded)
#' @id id column from site_id. siteiid is recommended if using a veg_df generated with ecositer functions for interoperability downstream
#' @param x x coordinate (i.e., easting or longitude) column from site_df
#' @param y y coordinate (i.e., norhting or latitude) column from site_df
#' @param EPSG the EPSG code for the coordinate reference system (CRS) of the input data, in format EPSG:36211
#'
#' @return dataframe of site
#' @export
#'
#' @examples
#' site_prism_annual_normals(prism_dir = "C:/Users/Nathan.Roe/Documents/PRISM_R/annual", id = siteiid, x = "utmeasting", y = "utmnorthing", EPSG = "EPSG:32611")
site_prism_annual_normals <- function(site_df, prism_dir, id, x, y, EPSG){

  # error handling
  if(!id %in% colnames(site_df)){
    stop("id is not a column in dataframe")
  }

  if(!x %in% colnames(site_df)){
    stop("x is not a column in dataframe")
  }

  if(!y %in% colnames(site_df)){
    stop("y is not a column in dataframe")
  }

  tryCatch({
    crs_info <- terra::crs(EPSG)
  },
  error = function(e){
    stop(sprintf("%s is not recognized. Please provide valid EPSG in format 'EPSG:32611'.", EPSG))
  }
  )

  # set directory where prism data is/will be located
  my_prism_dir <- prism_dir # used in multiple locations, so this is master assignment
  prism::prism_set_dl_dir(path = prism_dir)

  # prism properties, names, units
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

  # download prism data
  for(i in clim_vars){
    full_p <- file.path(my_prism_dir, i)
    dir.create(full_p, showWarnings = FALSE)
    prism::prism_set_dl_dir(full_p)
    prism::get_prism_normals(type = i,
                             resolution = "800m",
                             annual = TRUE,
                             keepZip = FALSE)
  }

  # reduce data frame to id and geo columns, only keep unique rows (currently duplicated because of species data)
  site_df <- site_df[, c(id, x, y)] |> unique()

  # create spatvector
  site_vect <- terra::vect(site_df, geom = c(x, y), crs = EPSG) |>
    terra::project("EPSG:6269")

  rast_extract <- lapply(seq_along(clim_vars), FUN = function(x){
    my_bil <- list.files(paste0(my_prism_dir, "/", clim_vars[x]),
                         full.names = TRUE,
                         recursive = TRUE,
                         pattern = ".bil$")

    my_extract <- terra::extract(x = terra::rast(my_bil),
                                 y = site_vect,
                                 ID = FALSE)

  })

  # column bind list
  site_clim <- do.call(cbind, rast_extract)
  # assign column names
  colnames(site_clim) <- clim_vars
  # add id column
  site_clim[[paste(id)]] <- site_df[[paste(id)]]
  # order columns with id first
  site_clim <- site_clim[,c(paste(id), clim_vars)]

  return(site_clim)

}
