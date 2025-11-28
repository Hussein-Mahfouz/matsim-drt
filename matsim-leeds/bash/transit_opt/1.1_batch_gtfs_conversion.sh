#!/bin/bash

# Convert GTFS feeds from transit_opt repo to MATSim format
# Run from matsim-leeds root: bash bash/transit_opt/1.1_batch_gtfs_conversion.sh <scenario_name>

set -e

# Scenario name can be: sc_avg_var, sc_peak_var, sc_sum_var, wt_avg_tot, wt_avg_var, wt_int_tot, 
#                       wt_int_tot, wt_int_var, wt_peak_tot, wt_peak_var, wt_sum_tot
SCENARIO_NAME=$1

# Path to transit_opt repo (adjust if needed)
TRANSIT_OPT_REPO="../../transit_opt"

# Input directory (GTFS feeds in transit_opt repo)
GTFS_INPUT_DIR="$TRANSIT_OPT_REPO/output/$SCENARIO_NAME/iteration_01/pso_results"

# Output directory in matsim-leeds
OUTPUT_DIR="data/supply/transit_opt_paper/$SCENARIO_NAME"

# PT2MATSim config template
PT2MATSIM_CONFIG="src/main/resources/config_pt2matsim.xml"

# GTFS conversion parameters
SERVICE_DATE="20230814"
CRS="EPSG:3857"

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Check if input directory exists
if [ ! -d "$GTFS_INPUT_DIR" ]; then
    echo "Error: GTFS input directory not found: $GTFS_INPUT_DIR"
    exit 1
fi

# Get the classpath
mvn dependency:build-classpath -Dmdep.outputFile=cp.txt
CLASSPATH=$(cat cp.txt):target/classes

echo "========================================="
echo "Converting GTFS feeds for: $SCENARIO_NAME"
echo "========================================="
echo "Input:  $GTFS_INPUT_DIR"
echo "Output: $OUTPUT_DIR"
echo ""

# Run batch GTFS converter
java -cp $CLASSPATH com.husseinmahfouz.matsim.pt2matsim.RunBatchGTFSConverter \
    "$GTFS_INPUT_DIR" \
    "$SERVICE_DATE" \
    "$CRS" \
    "$OUTPUT_DIR" \
    "$PT2MATSIM_CONFIG"

# Clean up extracted GTFS folders from transit_opt repo (the zip files are extracted in place)
# This command should only delete directories (not .zip or .json files)
echo ""
echo "Cleaning up extracted GTFS folders from transit_opt repo..."
find "$GTFS_INPUT_DIR" -type d -name "combined_solution_*" ! -name "*.zip" ! -name "*.json" -exec rm -rf {} + 2>/dev/null || true


echo ""
echo "========================================="
echo "GTFS conversion complete for $SCENARIO_NAME"
echo "========================================="