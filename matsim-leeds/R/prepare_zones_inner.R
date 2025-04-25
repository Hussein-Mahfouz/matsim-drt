# Basic script to delineate an inner DRRT zone based on population density
# Inspired by findings from (Bischoff et. al. 2018) https://www.sciencedirect.com/science/article/pii/S1877050918304290

library(tidyverse)
library(sf)
library(tmap)


crs_to_use = 3857

# OPTION 1: Service are basedon pop density

# ---------------- Read in study area
study_area = st_read("../../../drt-potential/data/interim/study_area_boundary.geojson") %>%
  st_transform(crs_to_use)

# --- pop density
oa_pop_density <- read.csv("../../../Lit-Review/transfer/images/plots/data_raw/census_2021_pop_density.csv", skip = 6) %>%
  rename(OA = X2021.output.area, pop_density = X2021) %>%
  select(OA, pop_density)

# add pop density to sf
study_area_pop <- study_area %>%
  inner_join(oa_pop_density, by = c("OA21CD" = "OA"))

# create pop density percentile column
study_area_pop = study_area_pop %>%
  mutate(pop_density_rank = percent_rank(pop_density))


# plot

tm_shape(study_area_pop) +
  tm_fill(title = "people/km2",
          col = "pop_density_rank",
          style = "jenks", # "quantile",
          palette = "Blues")

# What next?

# --- 1. filter by percentile threshold
# --- 2. Get coherent area - how?
    # Biggest coherent polygon?
    # Biggest n coherent polygons then st_convex_hull?



# ---------- Option 2: Buffer around central point (Leeds Train Station)

# List of buffer radii in metres
radii <- c(4000, 5000, 6000, 7000)

# Create buffers and save shapefiles
for (radius in radii) {
  # Create buffer
  drt_zone <- st_point(c(-1.5489263, 53.795155)) |>  # Point coordinates
    st_sfc(crs = 4326) |>                            # WGS84
    st_transform(3857) |>                            # Projected CRS
    st_buffer(dist = radius)

  # Define output folder and ensure it exists
  output_dir <- paste0("shapefiles/inner/drt_zone_", radius)
  dir.create(output_dir)

  # Save shapefile (always named drt_zone.shp)
  st_write(drt_zone, file.path(output_dir, "drt_zone.shp"),
           driver = "ESRI Shapefile", delete_layer = TRUE)
}
