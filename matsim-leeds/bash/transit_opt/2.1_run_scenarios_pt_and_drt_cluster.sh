#!/bin/bash

# This script submits a MATSim simulation job for each GTFS feed directory to the cluster using sbatch.
# Each GTFS feed directory should contain:
#   - schedule_mapped.xml.gz
#   - vehicles_unmapped.xml
#   - network_mapped.xml.gz
# Output for each run will be placed in a subdirectory "output/" inside each feed directory.
# The script dynamically sets input plans and vehicles files based on the SAMPLE_SIZE variable.

set -e

# --- Leeds HPC modules

module load gcc/14.2.0
module load java/jdk-21.0.6
# Load local Maven instead of module (Maven is not on Aire, so I installed locally)
export MAVEN_HOME="$HOME/maven"
export PATH="$MAVEN_HOME/bin:$PATH"

# Set the working directory (should be the project root: matsim-leeds)
MATSIM_DIR="$(pwd)"

# Directory containing GTFS feed subfolders
# FEEDS_PARENT_DIR="$MATSIM_DIR/data/external/gtfs_optimisation/min_variance_stops"
FEEDS_PARENT_DIR="$MATSIM_DIR/data/external/gtfs_optimisation/max_min_theoretical"

# Path to the template config file (relative to project root)
TEMPLATE_CONFIG="$MATSIM_DIR/src/main/resources/fleet_sizing/config_simulation_dmc_drt_100_feeder.xml"

# Path to the MATSim jar (relative to project root)
JAR_FILE="$MATSIM_DIR/target/matsim-leeds-1.0.jar"

# Main class to run
MAIN_CLASS="com.husseinmahfouz.matsim.RunDMCSimulationDRTMultipleGTFS"

# Simulation parameters
SAMPLE_SIZE="1.00"
ITERATIONS=55
USE_REJECTION_CONSTRAINT="true"
GLOBAL_THREADS=12
QSIM_THREADS=12

# Cluster resource parameters
CPUS_PER_TASK=12
MEM_PER_CPU=8192
MAX_RUNTIME="16:00:00"

for FEED_DIR in "$FEEDS_PARENT_DIR"/*/; do
    # Skip if not a directory
    [ -d "$FEED_DIR" ] || continue

    FEED_NAME=$(basename "$FEED_DIR")
    OUTPUT_DIR="${FEED_DIR}output"
    mkdir -p "$OUTPUT_DIR"

    # Dynamically set input plans and vehicles file based on sample size
    INPUT_PLANS_FILE="$MATSIM_DIR/data/demand/plans_sample_eqasim_${SAMPLE_SIZE}.xml"
    VEHICLES_FILE="$MATSIM_DIR/data/supply/network_vehicles_${SAMPLE_SIZE}.xml"

    # Paths to PT/DRT files in this feed directory
    TRANSIT_SCHEDULE_FILE="${FEED_DIR}schedule_mapped.xml.gz"
    TRANSIT_VEHICLES_FILE="${FEED_DIR}vehicles_unmapped.xml"
    NETWORK_FILE="${FEED_DIR}network_mapped.xml.gz"

    # Submit each job to the cluster
    sbatch -n 1 --cpus-per-task=$CPUS_PER_TASK --time=$MAX_RUNTIME --mem-per-cpu=$MEM_PER_CPU \
        --job-name="PTDRT_${FEED_NAME}" \
        --wrap="java -Xmx48G -cp $JAR_FILE $MAIN_CLASS \
            --config-path $TEMPLATE_CONFIG \
            --sample-size $SAMPLE_SIZE \
            --iterations $ITERATIONS \
            --use-rejection-constraint $USE_REJECTION_CONSTRAINT \
            --global-threads $GLOBAL_THREADS \
            --qsim-threads $QSIM_THREADS \
            --output-directory $OUTPUT_DIR \
            --input-plans-file $INPUT_PLANS_FILE \
            --vehicles-file $VEHICLES_FILE \
            --transit-schedule-file $TRANSIT_SCHEDULE_FILE \
            --transit-vehicles-file $TRANSIT_VEHICLES_FILE \
            --network-input-file $NETWORK_FILE"

    echo "Submitted job for $FEED_NAME"
done