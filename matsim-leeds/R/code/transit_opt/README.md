# Transit Optimization Analysis Scripts

## Overview

Scripts for analyzing transit optimization results from PSO-based network redesign.

---

## Execution Order

1. run_objectives.R (cluster)
2. process_gtfs_headways.R (cluster)
↓ Download outputs
3. evaluate_objectives.R (local)
4. plot_maps.R (local)



---

## Scripts

### Analysis Pipeline

**`run_objectives.R`**
- Calculates mode share and VKM for all optimization solutions
- Reads: `eqasim_trips.csv`, GTFS feeds, DRT zones
- Outputs: `mode_share_by_objective.csv`, `vkm_by_objective.csv`, `pso_objective_values.csv`
- Run on cluster: `bash bash/transit_opt/3_post_run_analysis_r.sh --cluster`

**`process_gtfs_headways.R`**
- Processes GTFS feeds to calculate headways and network changes
- Reads: Base and solution GTFS feeds, DRT deployment JSONs
- Outputs: `gtfs_sf_headways_overline.rds`, `gtfs_headway_comparisons_overline.rds`, `drt_fleet_deployments.csv`
- Run on cluster: `bash bash/transit_opt/4_process_gtfs_headways.sh --cluster`

**`evaluate_objectives.R`**
- Statistical analysis: correlations, top-k recall, scenario comparison, catchment sensitivity
- Reads: All CSV/RDS outputs from above
- Outputs: Tables and plots in `R/plots/transit_opt_paper/`
- Run: `source("R/code/transit_opt/evaluate_objectives.R")`

**`plot_maps.R`**
- Creates spatial visualizations of network changes
- Reads: GTFS headway RDS files, DRT deployments CSV
- Outputs: Map plots in `R/plots/transit_opt_paper/`
- Run: `source("R/code/transit_opt/plot_maps.R")`

---

## Helper Modules

**`mode_share_catchment.R`**
- Functions for calculating mode share within PT/DRT catchments
- Used by: `run_objectives.R`

**`vkm_catchment.R`**
- Functions for calculating vehicle kilometers traveled by mode
- Used by: `run_objectives.R`

**`gtfs_headway_analysis.R`**
- Functions for GTFS processing and headway calculation
- Used by: `process_gtfs_headways.R`, `plot_maps.R`

**`load_spatial_layers.R`**
- Functions for loading spatial data (study area, basemap, DRT zones)
- Used by: `plot_maps.R`

---

## Bash Scripts

**`bash/transit_opt/3_post_run_analysis_r.sh`**
- Runs `run_objectives.R` on cluster with SLURM

**`bash/transit_opt/4_process_gtfs_headways.sh`**
- Runs `process_gtfs_headways.R` on cluster with SLURM

---

## Quick Start

**On cluster:**
```bash
# First time setup
bash bash/transit_opt/3_post_run_analysis_r.sh --cluster --iteration iteration_02 --update-env
bash bash/transit_opt/4_process_gtfs_headways.sh --cluster --iteration iteration_02 --update-env

# Subsequent runs
bash bash/transit_opt/3_post_run_analysis_r.sh --cluster --iteration iteration_02
bash bash/transit_opt/4_process_gtfs_headways.sh --cluster --iteration iteration_02
