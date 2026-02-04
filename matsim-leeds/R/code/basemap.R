library(tidyverse)
library(sf)
library(tmap)


source("../../../drt-potential/R/study_area_geographies.R")


# ---------- LOAD IN DATASETS ---------- #

# ----- 1. administrative boundaries

study_area <- st_read(
  "../../../drt-potential/data/interim/study_area_boundary.geojson"
)

# convert to desired resolution
geography = "MSOA"
study_area2 = study_area_geographies(
  study_area = study_area,
  geography = geography
)


# ----- 2. RURAL URBAN AS BASE MAP

# --- 2011 map and urban rural classification
basemap <- st_read(
  "../../../drt-potential/data/external/oa_england_2011/OA_2011_EW_BGC_V2.shp"
) %>%
  st_transform(st_crs(study_area)) %>%
  st_make_valid()

urban_rural_csv <- read_csv(
  "../../../drt-potential/data/external/census_2011_rural_urban.csv"
)


# add numeric columns for coloring
urban_rural_csv <- urban_rural_csv %>%
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
  # turn column to factor
  mutate(RUC11CD_NM_FCT = as.factor(RUC11CD_NM))

# # edit text for legend
# urban_rural_csv <- urban_rural_csv %>%
#   mutate(RUC11 = case_when(RUC11 == "Rural hamlets and isolated dwellings" ~ "Rural hamlets and \nisolated dwellings",
#                            .default = RUC11))

# join
basemap_urban_rural <- basemap %>%
  left_join(urban_rural_csv, by = "OA11CD")


# --- filter to study area
# basemap_urban_rural <- basemap_urban_rural %>%
#   st_filter(st_union(study_area) %>%
#               st_make_valid(),
#             .predicate = st_intersects)

basemap_urban_rural <- basemap_urban_rural %>%
  st_intersection(st_union(study_area)) %>%
  st_make_valid()


# ----- 3. BUILT UP AREAS (for text)

bua <- st_read("../../../drt-potential/data/external/BUA_2022_GB.geojson") %>%
  st_transform(st_crs(study_area)) %>%
  st_make_valid() %>%
  st_filter(
    st_union(study_area) %>%
      st_make_valid(),
    .predicate = st_intersects
  )

# Remove Leeds and Clean up labels
bua2 <- bua %>%
  mutate(BUA22NM = stringr::str_replace(BUA22NM, "\\(Leeds\\)", "")) %>%
  # Keep only BUA polygons that actually intersect/overlap with the basemap
  # This prevents labels appearing in white space outside the study area
  st_filter(st_union(basemap_urban_rural), .predicate = st_within)

# ----------------------------- GTFS FEED

gtfs_bus <- st_read(
  "../../../drt-potential/data/interim/gtfs_freq/gtfs_bus_sf.geojson"
)

gtfs_bus_overline <- gtfs_bus %>%
  filter(scenario == "pt_wkday_morning") %>%
  mutate(headway_inv = (1 / headway_secs) * 3600) #%>%
#filter(headway_secs < 7200)

gtfs_bus_overline = stplanr::overline(
  gtfs_bus_overline,
  attrib = "headway_inv",
  fun = sum
)

gtfs_bus_overline = gtfs_bus_overline %>%
  mutate(headway_inv = round(headway_inv))


# ----------------------------- PLOTTING THE MAP

tm_shape(basemap_urban_rural) +
  # 1. Urbanisation Layer
  tm_fill(
    fill = "RUC11",
    fill.scale = tm_scale_categorical(values = "brewer.bu_pu"),
    fill.legend = tm_legend(
      title = "Level of\nUrbanisation\n(DEFRA 2011)",
      text.size = 1.2,
      bg.color = "transparent", # Make background transparent
      frame = FALSE # Remove border
    ),
    fill_alpha = 0.5
  ) +

  # 2. Bus Layer
  tm_shape(gtfs_bus_overline) +
  tm_lines(
    lwd = "headway_inv",
    col = "darkgreen",
    col_alpha = 0.7,
    lwd.scale = tm_scale_continuous(values.scale = 20),
    lwd.legend = tm_legend(
      title = "Buses/Hour (Morning period)",
      orientation = "landscape",
      bg.color = "transparent", # Make background transparent
      frame = FALSE, # Remove border
      height = 2.6 # Reduce vertical space (adjust as needed)
    )
  ) +

  # 3. Borders and Areas
  tm_shape(st_union(study_area)) +
  tm_borders(lwd = 1.2) +

  tm_shape(bua2) +
  tm_text(
    text = "BUA22NM",
    size = 0.75,
    remove.overlap = TRUE
  ) +

  # 4. Layout
  tm_title("Leeds City Region - Urban/Rural Classification") +
  tm_layout(
    text.fontfamily = 'Georgia',
    frame = FALSE,
    legend.position = tm_pos_in("left", "bottom"),
    legend.width = 5.5,
    legend.text.size = 2.5,
    legend.bg.color = "transparent", # Global transparent background
    legend.frame = FALSE # Global: no frames on legends
  ) +

  # 5. Side-by-Side Scalebar & Compass
  tm_scalebar(
    text.size = 0.6,
    position = c(0.80, 0.05)
  ) +
  tm_compass(
    size = 2,
    text.size = 0.8,
    position = c(0.95, 0.05)
  ) -> map_context

map_context

# Save
tmap_save(
  tm = map_context,
  filename = "plots/rural_urban_bus.png",
  width = 12,
  dpi = 600,
  asp = 0
)
