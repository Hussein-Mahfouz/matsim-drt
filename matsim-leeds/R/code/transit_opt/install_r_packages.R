#!/usr/bin/env Rscript

# Install R packages not available in conda

message("Installing additional R packages...")

packages_to_install <- c("tidytransit", "stplanr")

for (pkg in packages_to_install) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    message("Installing ", pkg, "...")
    install.packages(
      pkg,
      repos = "https://cloud.r-project.org",
      Ncpus = 4
    )
  } else {
    message(pkg, " already installed")
  }
}

message("Package installation complete")
