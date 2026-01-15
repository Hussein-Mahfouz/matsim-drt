#!/bin/bash

# Process GTFS headways for transit optimization
#
# USAGE:
#   bash bash/transit_opt/4_process_gtfs_headways.sh [OPTIONS]
#
# OPTIONS:
#   --cluster       Submit job to SLURM cluster (default: run locally)
#   --update-env    Update/Create the conda environment before running
#   --iteration ID  Specify iteration folder (default: iteration_01)
#
# EXAMPLES:
#   # Run locally for default iteration_01
#   bash bash/transit_opt/4_process_gtfs_headways.sh
#
#   # Run on cluster for iteration_02
#   bash bash/transit_opt/4_process_gtfs_headways.sh --cluster --iteration iteration_02
#
#   # First run (setup env + install extra packages)
#   bash bash/transit_opt/4_process_gtfs_headways.sh --cluster --update-env

# Default values
RUN_ON_CLUSTER=false
UPDATE_ENV=false
ITERATION="iteration_01"

# Parse arguments using a loop
while [[ $# -gt 0 ]]; do
    case $1 in
        --cluster)
            RUN_ON_CLUSTER=true
            shift # Remove --cluster from processing
            ;;
        --update-env)
            UPDATE_ENV=true
            shift # Remove --update-env from processing
            ;;
        --iteration)
            ITERATION="$2"
            shift 2 # Remove --iteration and its value
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

echo "Configuration:"
echo "  Cluster:   $RUN_ON_CLUSTER"
echo "  Iteration: $ITERATION"
echo "  Update Env: $UPDATE_ENV"

# Get the current working directory (matsim-leeds root)
MATSIM_DIR="$(pwd)"

# Path to R scripts and conda environment file
R_SCRIPT="$MATSIM_DIR/R/code/transit_opt/process_gtfs_headways.R"
R_INSTALL_SCRIPT="$MATSIM_DIR/R/code/transit_opt/install_r_packages.R"
CONDA_ENV_FILE="$MATSIM_DIR/R/r_transit_opt_env.yaml"
CONDA_ENV_NAME="r-transit-opt"

# Check if R script exists
if [ ! -f "$R_SCRIPT" ]; then
    echo "Error: R script not found at $R_SCRIPT"
    exit 1
fi

if [ "$RUN_ON_CLUSTER" = true ]; then
    echo "========================================="
    echo "Submitting GTFS headway processing to cluster ($ITERATION)"
    echo "========================================="
    
    # Check if conda environment file exists
    if [ ! -f "$CONDA_ENV_FILE" ]; then
        echo "Error: Conda environment file not found at $CONDA_ENV_FILE"
        exit 1
    fi
    
    # Load miniforge module
    module load miniforge/24.7.1
    
    # Handle environment creation/updating
    if [ "$UPDATE_ENV" = true ]; then
        echo "Updating conda environment: $CONDA_ENV_NAME"
        if conda env list | grep -q "^${CONDA_ENV_NAME} "; then
            echo "Environment exists, updating packages..."
            conda env update -n $CONDA_ENV_NAME -f "$CONDA_ENV_FILE" --prune
        else
            echo "Environment doesn't exist, creating..."
            conda env create -f "$CONDA_ENV_FILE"
        fi
        
        # Install additional R packages not in conda
        echo "Installing additional R packages..."
        source activate $CONDA_ENV_NAME
        Rscript "$R_INSTALL_SCRIPT"
    else
        # Check if environment exists, create if not
        if ! conda env list | grep -q "^${CONDA_ENV_NAME} "; then
            echo "Creating conda environment: $CONDA_ENV_NAME"
            conda env create -f "$CONDA_ENV_FILE"
            
            # Install additional R packages
            echo "Installing additional R packages..."
            source activate $CONDA_ENV_NAME
            Rscript "$R_INSTALL_SCRIPT"
        else
            echo "Using existing conda environment: $CONDA_ENV_NAME"
        fi
    fi
    
    # Cluster resource parameters
    CPUS_PER_TASK=6  # More CPUs for parallel overline processing
    MEM_PER_CPU=8192  # 8GB per CPU
    MAX_RUNTIME="8:00:00"  # 8 hours
    
    # Create logs directory
    LOG_DIR="$MATSIM_DIR/R/code/transit_opt/logs"
    mkdir -p "$LOG_DIR"
    
    # Submit job with conda environment activation
    sbatch -n 1 --cpus-per-task=$CPUS_PER_TASK \
        --time=$MAX_RUNTIME \
        --mem-per-cpu=$MEM_PER_CPU \
        --job-name="gtfs_headway_${ITERATION}" \
        --output="${LOG_DIR}/gtfs_headway_${ITERATION}-%j.out" \
        --wrap="module load miniforge/24.7.1 && \
                source activate $CONDA_ENV_NAME && \
                cd $MATSIM_DIR && \
                Rscript $R_SCRIPT $ITERATION"
    
    echo "Job submitted. Check logs in $LOG_DIR"
else
    echo "========================================="
    echo "Running GTFS headway processing locally ($ITERATION)"
    echo "========================================="
    
    if [ "$UPDATE_ENV" = true ]; then
        echo "Warning: --update-env flag only works with --cluster"
    fi
    
    # Run locally (assumes R is available in PATH)
    cd "$MATSIM_DIR"
    Rscript "$R_SCRIPT" "$ITERATION"
    
    echo ""
    echo "========================================="
    echo "Analysis complete"
    echo "========================================="
fi