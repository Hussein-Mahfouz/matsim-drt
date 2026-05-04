#!/bin/bash

# Convert GTFS feeds from transit_opt repo to MATSim format
# 
# Usage:
#   Local execution:    bash bash/transit_opt/1.1_batch_gtfs_conversion.sh <scenario_name> <iteration_number>
#   Cluster submission: bash bash/transit_opt/1.1_batch_gtfs_conversion.sh <scenario_name> <iteration_number> --cluster
#   (--internal-run is used internally by Slurm, never call it manually)

# Scenario name can be: sc_avg_var, sc_int_var, sc_peak_var, sc_sum_var, wt_avg_tot, wt_avg_var, wt_int_tot, 
#                       wt_int_tot, wt_int_var, wt_peak_tot, wt_peak_var, wt_sum_tot, wt_sum_var

# Run from matsim-leeds root: bash bash/transit_opt/1.1_batch_gtfs_conversion.sh <scenario_name> <iteration_number>

set -e

# ================= CONFIGURATION =================
if [ $# -lt 2 ]; then
    echo "Usage: $0 <scenario_name> <iteration_number> [--cluster]"
    echo "Examples:"
    echo "  Local:   $0 sc_avg_var 01"
    echo "  Cluster: $0 sc_avg_var 01 --cluster"
    exit 1
fi

SCENARIO_NAME=$1
ITERATION_NUMBER=$2
MODE="local"

# Check if user wants to submit to cluster
if [ "$3" == "--cluster" ]; then
    MODE="cluster"
# Check if we are currently running ON the cluster (internal flag)
elif [ "$3" == "--internal-run" ]; then
    MODE="internal"
fi

# Paths
TRANSIT_OPT_REPO="../../transit_opt"
GTFS_INPUT_DIR="$TRANSIT_OPT_REPO/output/$SCENARIO_NAME/iteration_${ITERATION_NUMBER}/pso_results"
OUTPUT_DIR="data/supply/transit_opt_paper/iteration_${ITERATION_NUMBER}/$SCENARIO_NAME"
PT2MATSIM_CONFIG="src/main/resources/config_pt2matsim.xml"
JAR_FILE="target/matsim-leeds-1.0.jar"

# GTFS conversion parameters
SERVICE_DATE="20230814"
CRS="EPSG:3857"

# ================= PHASE 1: SUBMISSION (Runs on Login Node) =================
if [ "$MODE" == "cluster" ]; then
    echo "========================================="
    echo "🚀 PREPARING CLUSTER SUBMISSION"
    echo "========================================="

    # 1. Validation
    if [ ! -f "$JAR_FILE" ]; then
        echo "❌ Error: JAR file not found at $JAR_FILE"
        echo "   Please run: mvn clean package -DskipTests"
        exit 1
    fi

    if [ ! -d "$GTFS_INPUT_DIR" ]; then
        echo "❌ Error: GTFS input directory not found: $GTFS_INPUT_DIR"
        exit 1
    fi

    # 2. Setup Logs
    LOG_DIR="bash/transit_opt/logs"
    mkdir -p "$LOG_DIR"

    # 3. Resources
    CPUS=4
    MEM_PER_CPU=8192  # 8GB per CPU = 32GB total
    TIME="02:00:00"

    echo "Submitting job for: $SCENARIO_NAME (iteration $ITERATION_NUMBER)"
    echo "  Resources: ${CPUS} CPUs, $((CPUS * MEM_PER_CPU / 1024))GB RAM"
    echo ""

    # 4. Submit THIS script to Slurm
    # Pass absolute path to avoid issues with working directory
    SCRIPT_PATH="$(cd "$(dirname "$0")" && pwd)/$(basename "$0")"
    
    sbatch -n 1 --cpus-per-task=$CPUS \
        --mem-per-cpu=$MEM_PER_CPU \
        --time=$TIME \
        --job-name="gtfs_${SCENARIO_NAME}_iter${ITERATION_NUMBER}" \
        --output="${LOG_DIR}/gtfs_conversion_${SCENARIO_NAME}_iter${ITERATION_NUMBER}_%j.out" \
        --error="${LOG_DIR}/gtfs_conversion_${SCENARIO_NAME}_iter${ITERATION_NUMBER}_%j.err" \
        "$SCRIPT_PATH" "$SCENARIO_NAME" "$ITERATION_NUMBER" "--internal-run"
        
    echo "✅ Job submitted!"
    echo "   Monitor: squeue --me"
    echo "   Logs:    ls -lh ${LOG_DIR}/gtfs_conversion_${SCENARIO_NAME}_iter${ITERATION_NUMBER}_*.out"
    exit 0
fi

# ================= PHASE 2: EXECUTION (Runs Locally OR on Cluster Node) =================

echo "========================================="
if [ "$MODE" == "internal" ]; then
    echo "⚙️  RUNNING ON CLUSTER NODE"
    
    # Load environment
    module load gcc/14.2.0
    module load java/jdk-21.0.6
    export MAVEN_HOME="$HOME/maven"
    export PATH="$MAVEN_HOME/bin:$PATH"
    
    # On cluster, use the Fat JAR
    CLASSPATH="$JAR_FILE"
    
else
    echo "⚙️  RUNNING LOCALLY"
    
    # Locally, use Maven to find dependencies
    mvn dependency:build-classpath -Dmdep.outputFile=cp.txt > /dev/null 2>&1
    CLASSPATH=$(cat cp.txt):target/classes
fi
echo "========================================="

# 1. Ensure Directories Exist
if [ ! -d "$GTFS_INPUT_DIR" ]; then
    echo "❌ Error: GTFS input directory not found: $GTFS_INPUT_DIR"
    exit 1
fi
mkdir -p "$OUTPUT_DIR"

# 2. Run Java Tool
echo "🚀 Converting GTFS feeds for: $SCENARIO_NAME (iteration $ITERATION_NUMBER)"
echo "   Input:  $GTFS_INPUT_DIR"
echo "   Output: $OUTPUT_DIR"
echo ""

java -Xmx24G -cp "$CLASSPATH" com.husseinmahfouz.matsim.pt2matsim.RunBatchGTFSConverter \
    "$GTFS_INPUT_DIR" \
    "$SERVICE_DATE" \
    "$CRS" \
    "$OUTPUT_DIR" \
    "$PT2MATSIM_CONFIG"

# 3. Copy GTFS zip files to output directories
echo ""
echo "📦 Post-processing: Copying GTFS feeds..."

COPIED_COUNT=0
for GTFS_ZIP in "$GTFS_INPUT_DIR"/combined_solution_*_gtfs.zip; do
    [ -f "$GTFS_ZIP" ] || continue
    
    FILENAME=$(basename "$GTFS_ZIP")
    
    # Extract solution number using bash regex
    if [[ $FILENAME =~ combined_solution_([0-9]+)_gtfs\.zip ]]; then
        SOLUTION_NUM="${BASH_REMATCH[1]}"
    else
        echo "   ⚠️  Could not parse: $FILENAME"
        continue
    fi
    
    echo "   Processing solution $SOLUTION_NUM..."
    
    TARGET_DIR="$OUTPUT_DIR/combined_solution_$SOLUTION_NUM"
    
    if [ -d "$TARGET_DIR" ]; then
        cp "$GTFS_ZIP" "$TARGET_DIR/gtfs_feed.zip"
        echo "     ✓ Copied"
        ((++COPIED_COUNT))  # Pre-increment prevents exit code 1
    else
        echo "     ⚠️  Target directory not found: $TARGET_DIR"
    fi
done

echo "   Copied $COPIED_COUNT GTFS feeds"

# 4. Cleanup extracted folders
echo ""
echo "🧹 Cleaning up temporary extracted folders..."

CLEANED_COUNT=0
for EXTRACTED_DIR in "$GTFS_INPUT_DIR"/combined_solution_*; do
    # Only delete directories (not .zip or .json files)
    if [ -d "$EXTRACTED_DIR" ] && [[ ! "$EXTRACTED_DIR" =~ \.(zip|json)$ ]]; then
        rm -rf "$EXTRACTED_DIR"
        ((++CLEANED_COUNT))  # Pre-increment here too just in case
    fi
done

echo "   Removed $CLEANED_COUNT temporary directories"

echo ""
echo "========================================="
echo "✅ GTFS conversion complete: $SCENARIO_NAME (iteration $ITERATION_NUMBER)"
echo "========================================="