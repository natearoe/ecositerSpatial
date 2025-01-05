my_fun <- function(era, property ){

}





# bucket <- "https://storage.googleapis.com/dsm-ft-climate-derivatives-dev-bucket/PRISM/1981-2010/"
# filename <- "annual_MAP_mm_800m.tif"
# my_ecosite <- "F018XI201CA"
#
# r <- terra::rast(
#   paste0("/vsicurl/", paste0(gcloud, filename))
# )
#
# eco_spatial <- soilDB::fetchSDA_spatial(my_ecosite, by.col="ecoclassid") |>
#   terra::vect() |> terra::project(terra::crs(r))
#
# t <- r |> terra::extract(eco_spatial,
#                          touches = TRUE,
#                          exact = TRUE)
#
# z <- r |> terra::extract(eco_spatial,
#                          touches = TRUE,
#                          exact = TRUE)
#
# test <- terra::crop(x = r, y = eco_spatial, mask = TRUE)
#
# test1 <- r |> terra::extract(eco_spatial,
#                     touches = TRUE,
#                     exact = TRUE)
#
# test1$prod <- test1$final_MAP_mm_800m * test1$fraction
#
#
# test2 <- terra::crop(
#   x = r,
#   y = eco_spatial,
#   mask = TRUE,
#   touches = TRUE
# ) |> terra::extract(y = eco_spatial,
#                     touches = TRUE,
#                     exact = TRUE)
#
# test2$prod <- test2$final_MAP_mm_800m * test2$fraction
#
# summary(test1$prod)
# summary(test2$prod)
#
#
#
# microbenchmark::microbenchmark(
#   r |> terra::extract(eco_spatial,
#                       touches = TRUE,
#                       exact = TRUE),
#   terra::crop(
#     x = r,
#     y = eco_spatial,
#     mask = TRUE,
#     touches = TRUE
#   ) |> terra::extract(y = eco_spatial,
#                       touches = TRUE,
#                       exact = TRUE)
# )



