#!/bin/bash

# Submit MATSim simulation jobs for all combined_solution_* directories in a scenario to the cluster
# Each solution directory contains:
#   - config_simulation_drt_feeder.xml
#   - network_mapped.xml.gz
#   - schedule_mapped.xml.gz
#   - vehicles_unmapped.xml
#   - drt_fleet_ne_merged.xml
#   - drt_fleet_nw_merged.xml

# Run from matsim-leeds root: bash bash/transit_opt/2.1_run_scenarios_pt_and_drt_cluster.sh <scenario_name>

if [ $# -ne 1 ]; then
    echo "Usage: $0 <scenario_name>"
    echo "Example: $0 sc_avg_var"
    exit 1
fi

SCENARIO_NAME=$1

# --- Leeds HPC modules 
module load gcc/14.2.0
module load java/jdk-21.0.6
# Load local Maven if needed
export MAVEN_HOME="$HOME/maven"
export PATH="$MAVEN_HOME/bin:$PATH"

# Get the current working directory (matsim-leeds root)
MATSIM_DIR="$(pwd)"

# Base directory for this scenario
SCENARIO_DIR="$MATSIM_DIR/data/supply/transit_opt_paper/$SCENARIO_NAME"

# Path to your MATSim jar
JAR_FILE="$MATSIM_DIR/target/matsim-leeds-1.0.jar"

# Main class (with DRT)
MAIN_CLASS="com.husseinmahfouz.matsim.RunDMCSimulationDRTMultipleGTFS"

# Simulation parameters
SAMPLE_SIZE="0.05"
ITERATIONS=55
USE_REJECTION_CONSTRAINT="true"
GLOBAL_THREADS=12
QSIM_THREADS=12

# Cluster resource parameters (TODO: learn if MATSim is memory-bound or cpu-bound)
CPUS_PER_TASK=12
MEM_PER_CPU=8192  # MB per CPU (8GB)
MAX_RUNTIME="16:00:00"  # 16 hours

# Shared input files (based on sample size)
INPUT_PLANS_FILE="$MATSIM_DIR/data/demand/plans_sample_eqasim_${SAMPLE_SIZE}.xml"
VEHICLES_FILE="$MATSIM_DIR/data/supply/network_vehicles_${SAMPLE_SIZE}.xml"

echo "========================================="
echo "Submitting jobs for scenario: $SCENARIO_NAME"
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
    
    # Submit job to cluster
    sbatch -n 1 --cpus-per-task=$CPUS_PER_TASK \
        --time=$MAX_RUNTIME \
        --mem-per-cpu=$MEM_PER_CPU \
        --job-name="PTDRT_${SCENARIO_NAME}_${SOLUTION_NAME}" \
        --wrap="java -Xmx80G -cp $JAR_FILE $MAIN_CLASS \
            --config-path $TEMPLATE_CONFIG \
            --global-threads $GLOBAL_THREADS \
            --qsim-threads $QSIM_THREADS \
            --use-rejection-constraint $USE_REJECTION_CONSTRAINT \
            --iterations $ITERATIONS \
            --sample-size $SAMPLE_SIZE \
            --output-directory $OUTPUT_DIR \
            --input-plans-file $INPUT_PLANS_FILE \
            --vehicles-file $VEHICLES_FILE \
            --transit-schedule-file $TRANSIT_SCHEDULE_FILE \
            --transit-vehicles-file $TRANSIT_VEHICLES_FILE \
            --network-input-file $NETWORK_FILE"

    echo "Submitted job for $SOLUTION_NAME"
done

echo ""
echo "========================================="
echo "All jobs submitted for: $SCENARIO_NAME"
echo "========================================="