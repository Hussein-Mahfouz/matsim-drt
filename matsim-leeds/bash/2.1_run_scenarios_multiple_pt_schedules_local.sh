#!/bin/bash

# This script runs MATSim simulations for multiple GTFS feeds located in subdirectories.
# Each GTFS feed directory should contain the necessary mapped schedule, vehicles, and network files.
# Adjust the paths and parameters as needed.
set -e

# Get the current working directory (assuming you run this script from the matsim-leeds directory)
MATSIM_DIR="$(pwd)"   # This automatically sets the current directory to MATSIM_DIR
# Path to the parent directory containing GTFS feed folders
FEEDS_PARENT_DIR="$MATSIM_DIR/data/external/gtfs_optimisation/min_variance_stops"

# Path to the template config file
# With DRT
# TEMPLATE_CONFIG="$MATSIM_DIR/src/main/resources/fleet_sizing/config_simulation_dmc_drt_100_feeder.xml"
# No DRT: Quicker for testing
TEMPLATE_CONFIG="$MATSIM_DIR/src/main/resources/config_simulation_dmc.xml"

# Path to your MATSim jar
JAR_FILE="$MATSIM_DIR/target/matsim-leeds-1.0.jar"

# Main class
## Without DRT
MAIN_CLASS="com.husseinmahfouz.matsim.RunDMCSimulationDRTMultipleGTFSNoDRT"
## With DRT
# MAIN_CLASS="com.husseinmahfouz.matsim.RunDMCSimulationDRTMultipleGTFS"  


# Other parameters
SAMPLE_SIZE="0.01"
ITERATIONS=5
USE_REJECTION_CONSTRAINT="true"
GLOBAL_THREADS=8
QSIM_THREADS=8

for FEED_DIR in "$FEEDS_PARENT_DIR"/*/; do
    # Skip if not a directory
    [ -d "$FEED_DIR" ] || continue

    FEED_NAME=$(basename "$FEED_DIR")
    OUTPUT_DIR="${FEED_DIR}output"

    # Ensure output directory exists
    mkdir -p "$OUTPUT_DIR"

    # Paths to relevant files in this feed directory
    TRANSIT_SCHEDULE_FILE="${FEED_DIR}schedule_mapped.xml.gz"
    TRANSIT_VEHICLES_FILE="${FEED_DIR}vehicles_unmapped.xml"
    NETWORK_FILE="${FEED_DIR}network_mapped.xml.gz"

    # Run the simulation
    # java -Xmx48G -cp "$JAR_FILE" $MAIN_CLASS \
    #     --config-path "$TEMPLATE_CONFIG" \
    #     --sample-size "$SAMPLE_SIZE" \
    #     --iterations "$ITERATIONS" \
    #     --use-rejection-constraint "$USE_REJECTION_CONSTRAINT" \
    #     --global-threads "$GLOBAL_THREADS" \
    #     --qsim-threads "$QSIM_THREADS" \
    #     --output-directory "$OUTPUT_DIR" \
    #     --transit-schedule-file "$TRANSIT_SCHEDULE_FILE" \
    #     --transit-vehicles-file "$TRANSIT_VEHICLES_FILE" \
    #     --network-input-file "$NETWORK_FILE"

      # Run the simulation (No DRT so no rejection constraint)
    java -Xmx48G -cp "$JAR_FILE" $MAIN_CLASS \
        --config-path "$TEMPLATE_CONFIG" \
        --sample-size "$SAMPLE_SIZE" \
        --iterations "$ITERATIONS" \
        --global-threads "$GLOBAL_THREADS" \
        --qsim-threads "$QSIM_THREADS" \
        --output-directory "$OUTPUT_DIR" \
        --transit-schedule-file "$TRANSIT_SCHEDULE_FILE" \
        --transit-vehicles-file "$TRANSIT_VEHICLES_FILE" \
        --network-input-file "$NETWORK_FILE"


    echo "Completed run for $FEED_NAME"
done