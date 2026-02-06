#!/usr/bin/env Rscript

# Install R packages not available in conda

message("Installing additional R packages...")

# Set repos
repos <- "https://cloud.r-project.org"

# Configure compiler flags to use conda environment libraries
conda_prefix <- Sys.getenv("CONDA_PREFIX")
if (conda_prefix != "") {
  message("Using conda environment: ", conda_prefix)

  # Set environment variables for package compilation
  Sys.setenv(
    PROJ_LIB = file.path(conda_prefix, "share/proj"),
    GDAL_DATA = file.path(conda_prefix, "share/gdal"),
    PKG_CONFIG_PATH = file.path(conda_prefix, "lib/pkgconfig")
  )
}

packages_to_install <- c("lwgeom", "tidytransit", "stplanr")

for (pkg in packages_to_install) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    message("Installing ", pkg, "...")
    install.packages(
      pkg,
      repos = repos,
      Ncpus = 4,
      configure.args = c(
        paste0("--with-proj-include=", file.path(conda_prefix, "include")),
        paste0("--with-proj-lib=", file.path(conda_prefix, "lib"))
      )
    )
  } else {
    message(pkg, " already installed")
  }
}

message("\nPackage installation complete")

# Verify installations
message("\nVerifying installations:")
for (pkg in packages_to_install) {
  if (require(pkg, character.only = TRUE, quietly = TRUE)) {
    message("✓ ", pkg, " loaded successfully")
  } else {
    message("✗ ", pkg, " FAILED to load")
  }
}
