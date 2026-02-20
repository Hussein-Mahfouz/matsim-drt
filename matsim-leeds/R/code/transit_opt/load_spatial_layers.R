library(tidyverse)
library(sf)

#' Load study area boundary
#'
#' @param path Path to study area geojson
#' @param crs Target CRS (default: 3857)
#'
#' @return sf object
#'
load_study_area <- function(
  path = "data/external/study_area_boundary.geojson",
  crs = 3857
) {
  message("Loading study area boundary...")
  st_read(path, quiet = TRUE) |>
    st_union() |>
    st_transform(crs) |>
    st_as_sf()
}


#' Load urban/rural basemap
#'
#' @param basemap_path Path to OA shapefile
#' @param urban_rural_path Path to urban/rural CSV
#' @param study_area sf object to clip to
#'
#' @return sf object with urban/rural classification
#'
load_basemap_urban_rural <- function(
  basemap_path = "../../drt-potential/data/external/oa_england_2011/OA_2011_EW_BGC_V2.shp",
  urban_rural_path = "../../drt-potential/data/external/census_2011_rural_urban.csv",
  study_area
) {
  message("Loading basemap and urban/rural classification...")

  basemap <- st_read(basemap_path, quiet = TRUE) %>%
    st_transform(st_crs(study_area)) %>%
    st_make_valid()

  urban_rural_csv <- read_csv(urban_rural_path, show_col_types = FALSE) %>%
    mutate(
      RUC11CD_NM = case_when(
        RUC11 == "Urban major conurbation" ~ 10,
        RUC11 == "Urban minor conurbation" ~ 9,
        RUC11 == "Urban city and town" ~ 8,
        RUC11 == "Urban city and town in a sparse setting" ~ 7,
        RUC11 == "Rural town and fringe" ~ 6,
        RUC11 == "Rural town and fringe in a sparse setting" ~ 5,
        RUC11 == "Rural village" ~ 4,
        RUC11 == "Rural village in a sparse setting" ~ 3,
        RUC11 == "Rural hamlets and isolated dwellings" ~ 2,
        RUC11 == "Rural hamlets and isolated dwellings in a sparse setting" ~ 1
      )
    ) %>%
    arrange(RUC11CD_NM) %>%
    mutate(RUC11CD_NM_FCT = as.factor(RUC11CD_NM))

  basemap_joined <- basemap %>%
    left_join(urban_rural_csv, by = "OA11CD")

  message("Clipping basemap to study area...")
  basemap_joined %>%
    st_intersection(
      st_union(study_area) %>% st_make_valid()
    )
}


#' Load DRT zones
#'
#' @param ne_path Path to NE zone shapefile
#' @param nw_path Path to NW zone shapefile
#' @param crs Target CRS (default: 3857)
#'
#' @return sf object with both DRT zones
#'
load_drt_zones <- function(
  ne_path = "data/supply/drt/ne_cluster_08_00_11_00.shp",
  nw_path = "data/supply/drt/nw_cluster_08_00_11_00.shp",
  crs = 3857
) {
  message("Loading DRT zones...")

  drt_ne <- st_read(ne_path, quiet = TRUE) |>
    st_transform(crs) %>%
    mutate(scenario = "drtNE")

  drt_nw <- st_read(nw_path, quiet = TRUE) |>
    st_transform(crs) %>%
    mutate(scenario = "drtNW")

  bind_rows(drt_ne, drt_nw)
}


#' Load all spatial layers
#'
#' @return List with study_area, basemap_urban_rural, drt
#'
load_all_spatial_layers <- function() {
  study_area <- load_study_area()
  basemap_urban_rural <- load_basemap_urban_rural(study_area = study_area)
  drt <- load_drt_zones()

  list(
    study_area = study_area,
    basemap_urban_rural = basemap_urban_rural,
    drt = drt
  )
}
