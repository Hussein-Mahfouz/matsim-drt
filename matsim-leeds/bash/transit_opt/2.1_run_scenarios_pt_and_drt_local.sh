#!/bin/bash

# Run MATSim simulations for all combined_solution_* directories in a scenario
# Each solution directory contains:
#   - config_simulation_drt_feeder.xml
#   - network_mapped.xml.gz
#   - schedule_mapped.xml.gz
#   - vehicles_unmapped.xml
#   - drt_fleet_ne_merged.xml
#   - drt_fleet_nw_merged.xml

# Run from matsim-leeds root: bash bash/transit_opt/2.1_run_scenarios_pt_and_drt_local.sh <scenario_name>

if [ $# -ne 1 ]; then
    echo "Usage: $0 <scenario_name>"
    echo "Example: $0 sc_avg_var"
    exit 1
fi

SCENARIO_NAME=$1

# Get the current working directory (matsim-leeds root)
MATSIM_DIR="$(pwd)"

# Base directory for this scenario
SCENARIO_DIR="$MATSIM_DIR/data/supply/transit_opt_paper/$SCENARIO_NAME"

# Path to your MATSim jar
JAR_FILE="$MATSIM_DIR/target/matsim-leeds-1.0.jar"

# Main class (with DRT)
MAIN_CLASS="com.husseinmahfouz.matsim.RunDMCSimulationDRTMultipleGTFS"

# Simulation parameters
SAMPLE_SIZE="0.01"
ITERATIONS=5
GLOBAL_THREADS=8
QSIM_THREADS=8

########################
# Parameters related to DRT rejections
########################

# 1. INDIVIDUAL LEVEL: Rejection Constraint parameters (Bayesian smoothing)
# If applied, it checks how many times a person has been rejected in previous iterations, 
# and probabilistically determines whether to make DRT mode available to person depending on rejection rate
USE_REJECTION_CONSTRAINT="true"
PRIOR_REQUESTS="10"        # Virtual prior attempts (default: 10)
PRIOR_REJECTIONS="1"       # Virtual prior rejections (default: 1, gives 10% base rate)
MIN_ATTEMPTS="3"           # Grace period attempts (default: 3)

# 2. GLOBAL LEVEL: DRT parameters (to control rejection rate) See issue #55
# Tries to match global DRT rejection rate with the rate specified below. Done by 
# adding a penalty to the DRT mode utility
ENABLE_REJECTION_PENALTY="true"  # Set to "false" to disable
TARGET_REJECTION_RATE="0.03"  # 3% target
CONTROLLER_GAIN="1.0"         # Proportional gain


# Shared input files (based on sample size)
INPUT_PLANS_FILE="$MATSIM_DIR/data/demand/plans_sample_eqasim_${SAMPLE_SIZE}.xml"
VEHICLES_FILE="$MATSIM_DIR/data/supply/network_vehicles_${SAMPLE_SIZE}.xml"

echo "========================================="
echo "Running simulations for scenario: $SCENARIO_NAME"
echo "========================================="

# Loop through each combined_solution_* directory
for SOLUTION_DIR in "$SCENARIO_DIR"/combined_solution_*/; do
    # Skip if not a directory
    [ -d "$SOLUTION_DIR" ] || continue

    SOLUTION_NAME=$(basename "$SOLUTION_DIR")
    
    # Check if config exists
    TEMPLATE_CONFIG="${SOLUTION_DIR}config_simulation_drt_feeder.xml"
    if [ ! -f "$TEMPLATE_CONFIG" ]; then
        echo "⚠️  Config not found, skipping: $SOLUTION_NAME"
        continue
    fi
    
    # PT/Network files in solution directory
    TRANSIT_SCHEDULE_FILE="${SOLUTION_DIR}schedule_mapped.xml.gz"
    TRANSIT_VEHICLES_FILE="${SOLUTION_DIR}vehicles_unmapped.xml"
    NETWORK_FILE="${SOLUTION_DIR}network_mapped.xml.gz"
    
    # Output directory
    OUTPUT_DIR="${SOLUTION_DIR}output"
    mkdir -p "$OUTPUT_DIR"
    
    echo ""
    echo "Running: $SOLUTION_NAME"
    echo "  Config: $TEMPLATE_CONFIG"
    echo "  Output: $OUTPUT_DIR"

    # Run the simulation
    java -Xmx48G -cp "$JAR_FILE" $MAIN_CLASS \
        --config-path "$TEMPLATE_CONFIG" \
        --global-threads "$GLOBAL_THREADS" \
        --qsim-threads "$QSIM_THREADS" \
        --use-rejection-constraint "$USE_REJECTION_CONSTRAINT" \
        --iterations "$ITERATIONS" \
        --sample-size "$SAMPLE_SIZE" \
        --output-directory "$OUTPUT_DIR" \
        --input-plans-file "$INPUT_PLANS_FILE" \
        --vehicles-file "$VEHICLES_FILE" \
        --transit-schedule-file "$TRANSIT_SCHEDULE_FILE" \
        --transit-vehicles-file "$TRANSIT_VEHICLES_FILE" \
        --network-input-file "$NETWORK_FILE"

    if [ $? -eq 0 ]; then
        echo "  ✓ Completed: $SOLUTION_NAME"
    else
        echo "  ❌ Failed: $SOLUTION_NAME"
    fi
done

echo ""
echo "========================================="
echo "All simulations completed for: $SCENARIO_NAME"
echo "========================================="