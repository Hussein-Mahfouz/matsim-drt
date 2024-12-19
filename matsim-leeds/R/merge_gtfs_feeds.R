library(tidyverse)
library(gtfstools)


# ----- load in the feeds

# specify directory
gtfs_dir = "../data/external/"
# get names of all zip files in directory
feeds = dir(gtfs_dir, ".zip$", full.names = TRUE)
# read them in
gtfs_feeds <- purrr::map(feeds, gtfstools::read_gtfs)

# ----- merge the gtfs feeds
gtfs_feeds_merged = gtfstools::merge_gtfs(gtfs_feeds)
# gtfs_feeds_merged = gtfstools::merge_gtfs(gtfs_feeds, prefix = c("bus", "rail"))

# ----- save the merged feed

write_gtfs(gtfs_feeds_merged, paste0(gtfs_dir, "study_area_gtfs_merged.zip"))


