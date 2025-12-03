#!/bin/bash

# Submit MATSim simulation jobs for all combined_solution_* directories in a scenario to the cluster
# Each solution directory contains:
#   - config_simulation_drt_feeder.xml
#   - network_mapped.xml.gz
#   - schedule_mapped.xml.gz
#   - vehicles_unmapped.xml
#   - drt_fleet_ne_merged.xml
#   - drt_fleet_nw_merged.xml

# Run from matsim-leeds root: 
#   All solutions: bash bash/transit_opt/2.1_run_scenarios_pt_and_drt_cluster.sh <scenario_name>
#   One solution:  bash bash/transit_opt/2.1_run_scenarios_pt_and_drt_cluster.sh <scenario_name> <solution_name>

if [ $# -lt 1 ] || [ $# -gt 2 ]; then
    echo "Usage: $0 <scenario_name> [solution_name]"
    echo "Examples:"
    echo "  All solutions: $0 sc_avg_var"
    echo "  One solution:  $0 sc_avg_var combined_solution_40"
    exit 1
fi

SCENARIO_NAME=$1
SPECIFIC_SOLUTION="${2:-}"  # Optional: if provided, only run this solution

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
SAMPLE_SIZE="1.00" # 0.50, 0.20, 0.10, 0.05, 0.01
ITERATIONS=65
GLOBAL_THREADS=12
QSIM_THREADS=12
CLEAN_ITERS_AT_END="keep"  # Delete the ITERS/ directory? Options (keep, delete). If I delete, eqasim does not write its csv files

# Cluster resource parameters (TODO: learn if MATSim is memory-bound or cpu-bound)
CPUS_PER_TASK=12
MEM_PER_CPU=8192  # MB per CPU (8GB)
MAX_RUNTIME="36:00:00"  # 36:00:00 = 36 hours

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
if [ -n "$SPECIFIC_SOLUTION" ]; then
    echo "Submitting job for: $SCENARIO_NAME / $SPECIFIC_SOLUTION"
else
    echo "Submitting jobs for all solutions in: $SCENARIO_NAME"
fi
echo "========================================="

# Determine which solutions to process
if [ -n "$SPECIFIC_SOLUTION" ]; then
    # Run only the specified solution
    SOLUTION_DIRS=("$SCENARIO_DIR/$SPECIFIC_SOLUTION")
    
    # Verify it exists
    if [ ! -d "${SOLUTION_DIRS[0]}" ]; then
        echo "❌ Error: Solution directory not found: ${SOLUTION_DIRS[0]}"
        exit 1
    fi
else
    # Run all solutions
    SOLUTION_DIRS=("$SCENARIO_DIR"/combined_solution_*/)
fi

# Loop through solution directories
for SOLUTION_DIR in "${SOLUTION_DIRS[@]}"; do
    # Skip if not a directory
    [ -d "$SOLUTION_DIR" ] || continue

    SOLUTION_NAME=$(basename "$SOLUTION_DIR")
    
    # Check if config exists
    TEMPLATE_CONFIG="${SOLUTION_DIR}/config_simulation_drt_feeder.xml"
    if [ ! -f "$TEMPLATE_CONFIG" ]; then
        echo "⚠️  Config not found, skipping: $SOLUTION_NAME"
        continue
    fi
    
    # PT/Network files in solution directory
    TRANSIT_SCHEDULE_FILE="${SOLUTION_DIR}/schedule_mapped.xml.gz"
    TRANSIT_VEHICLES_FILE="${SOLUTION_DIR}/vehicles_unmapped.xml"
    NETWORK_FILE="${SOLUTION_DIR}/network_mapped.xml.gz"
    
    # Define output directory
    OUTPUT_DIR="${SOLUTION_DIR}/output"

    # Separate directory for SLURM logs (persistent across runs) - OUTPUT_DIR is deleted by 
    # MATSim deleteDirectoryifExists
    SLURM_LOG_DIR="${SOLUTION_DIR}/logs"
    mkdir -p "$SLURM_LOG_DIR"

    # Submit job to cluster
    sbatch -n 1 --cpus-per-task=$CPUS_PER_TASK \
        --time=$MAX_RUNTIME \
        --mem-per-cpu=$MEM_PER_CPU \
        --job-name="${SCENARIO_NAME}_${SOLUTION_NAME}" \
        --output="${SLURM_LOG_DIR}/slurm-%j.out" \
        --wrap="java -Xmx80G -cp $JAR_FILE $MAIN_CLASS \
            --config-path $TEMPLATE_CONFIG \
            --global-threads $GLOBAL_THREADS \
            --qsim-threads $QSIM_THREADS \
            --use-rejection-constraint $USE_REJECTION_CONSTRAINT \
            --iterations $ITERATIONS \
            --sample-size $SAMPLE_SIZE \
            --clean-iters-at-end $CLEAN_ITERS_AT_END \
            --output-directory $OUTPUT_DIR \
            --input-plans-file $INPUT_PLANS_FILE \
            --vehicles-file $VEHICLES_FILE \
            --transit-schedule-file $TRANSIT_SCHEDULE_FILE \
            --transit-vehicles-file $TRANSIT_VEHICLES_FILE \
            --network-input-file $NETWORK_FILE \
            --prior-requests $PRIOR_REQUESTS \
            --prior-rejections $PRIOR_REJECTIONS \
            --min-attempts $MIN_ATTEMPTS \
            --enable-rejection-penalty $ENABLE_REJECTION_PENALTY \
            --target-rejection-rate $TARGET_REJECTION_RATE \
            --controller-gain $CONTROLLER_GAIN"

    echo "Submitted job for $SOLUTION_NAME"
done

echo ""
echo "========================================="
if [ -n "$SPECIFIC_SOLUTION" ]; then
    echo "Job submitted for: $SPECIFIC_SOLUTION"
else
    echo "All jobs submitted for: $SCENARIO_NAME"
fi
echo "========================================="