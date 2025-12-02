#!/bin/bash

# Run R analysis for transit optimization objectives
# Local usage: bash bash/transit_opt/3_post_run_analysis.sh
# HPC usage: bash bash/transit_opt/3_post_run_analysis.sh --cluster

# Parse arguments
RUN_ON_CLUSTER=false
if [ "$1" == "--cluster" ]; then
    RUN_ON_CLUSTER=true
fi

# Get the current working directory (matsim-leeds root)
MATSIM_DIR="$(pwd)"

# Path to R script and conda environment file
R_SCRIPT="$MATSIM_DIR/R/code/transit_opt/run_objectives.R"
CONDA_ENV_FILE="$MATSIM_DIR/R/r_transit_opt_env.yaml"
CONDA_ENV_NAME="r-transit-opt"

# Check if R script exists
if [ ! -f "$R_SCRIPT" ]; then
    echo "Error: R script not found at $R_SCRIPT"
    exit 1
fi

if [ "$RUN_ON_CLUSTER" = true ]; then
    echo "========================================="
    echo "Submitting R analysis job to cluster"
    echo "========================================="
    
    # Check if conda environment file exists
    if [ ! -f "$CONDA_ENV_FILE" ]; then
        echo "Error: Conda environment file not found at $CONDA_ENV_FILE"
        exit 1
    fi
    
    # Load Anaconda module
    module load miniforge/24.7.1
    
    # Check if environment exists, create if not
    if ! conda env list | grep -q "^${CONDA_ENV_NAME} "; then
        echo "Creating conda environment: $CONDA_ENV_NAME"
        conda env create -f "$CONDA_ENV_FILE"
    fi
    
    # Cluster resource parameters
    CPUS_PER_TASK=4
    MEM_PER_CPU=16384  # MB per CPU (8GB)
    MAX_RUNTIME="12:00:00"  # 12 hours
    
    # Create logs directory
    LOG_DIR="$MATSIM_DIR/R/code/transit_opt/logs"
    mkdir -p "$LOG_DIR"
    
    # Submit job with conda environment activation
    sbatch -n 1 --cpus-per-task=$CPUS_PER_TASK \
        --time=$MAX_RUNTIME \
        --mem-per-cpu=$MEM_PER_CPU \
        --job-name="transit_opt_analysis" \
        --output="${LOG_DIR}/slurm-%j.out" \
        --wrap="module load miniforge/24.7.1 && \
                source activate $CONDA_ENV_NAME && \
                cd $MATSIM_DIR && \
                Rscript $R_SCRIPT"
    
    echo "Job submitted. Check logs in $LOG_DIR"
else
    echo "========================================="
    echo "Running R analysis locally"
    echo "========================================="
    
    # Run locally (assumes R is available in PATH)
    cd "$MATSIM_DIR"
    Rscript "$R_SCRIPT"
    
    echo ""
    echo "========================================="
    echo "Analysis complete"
    echo "========================================="
fi