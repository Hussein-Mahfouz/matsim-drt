#!/bin/bash

# Generate MATSim configs with dynamic DRT fleets based on optimization results

# This script combines the logic of AdaptConfigForDrt.sh and AdaptConfigForFeederDrt.sh, but is used
# to create dynamic fleets (fleets that change in size throughout the day) based on optimization results stored in JSON files.

# Steps:
    # 1a. Look at the drt json file of each solution, and get the corresponding drt fleet xml files
        # Each JSON file contains fleet deployment information for two DRT zones (NE and NW) across 6 time intervals.
    # 1b. Combine DRT fleet xml files per time period into one xml file
    # 2. Run AdaptConfigforDRT and AdaptConfigforFeederDRT to prepare MATSim config for simulation with feeder DRT 


# Usage:
#   Local execution:    bash bash/transit_opt/1.2b_generate_configs_per_scenario.sh <scenario_name> <iteration_number>
#   Cluster submission: bash bash/transit_opt/1.2b_generate_configs_per_scenario.sh <scenario_name> <iteration_number> --cluster
#   (--internal-run is used internally by Slurm, never call it manually)

# Run from matsim-leeds root: bash bash/transit_opt/1.2b_generate_scenario_configs.sh <scenario_name>

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

# Check execution mode
if [ "$3" == "--cluster" ]; then
    MODE="cluster"
elif [ "$3" == "--internal-run" ]; then
    MODE="internal"
fi

# Paths
TRANSIT_OPT_REPO="../../transit_opt"
JSON_INPUT_DIR="$TRANSIT_OPT_REPO/output/$SCENARIO_NAME/iteration_${ITERATION_NUMBER}/pso_results"
BASE_OUTPUT_DIR="data/supply/transit_opt_paper/iteration_${ITERATION_NUMBER}/$SCENARIO_NAME"
DRT_VEHICLES_DIR="data/supply/transit_opt_paper/drt_fleet_templates"
TEMPLATE_CONFIG="src/main/resources/config_simulation_dmc.xml"
JAR_FILE="target/matsim-leeds-1.0.jar"

# ================= PHASE 1: SUBMISSION =================
if [ "$MODE" == "cluster" ]; then
    echo "========================================="
    echo "🚀 PREPARING CLUSTER SUBMISSION"
    echo "========================================="

    # Validation
    if [ ! -f "$JAR_FILE" ]; then
        echo "❌ Error: JAR file not found at $JAR_FILE"
        echo "   Please run: mvn clean package -DskipTests"
        exit 1
    fi
    if [ ! -d "$JSON_INPUT_DIR" ]; then
        echo "❌ Error: JSON input directory not found: $JSON_INPUT_DIR"
        exit 1
    fi
    if [ ! -d "$DRT_VEHICLES_DIR" ]; then
        echo "❌ Error: DRT vehicle templates not found: $DRT_VEHICLES_DIR"
        echo "   Please run: bash bash/transit_opt/1.2a_create_drt_vehicles.sh"
        exit 1
    fi

    # Logs & Resources
    LOG_DIR="bash/transit_opt/logs"
    mkdir -p "$LOG_DIR"
    CPUS=2
    MEM_PER_CPU=16384  # 16GB per CPU = 32GB total
    TIME="01:00:00"

    echo "Submitting job for: $SCENARIO_NAME (iteration $ITERATION_NUMBER)"
    echo "  Resources: ${CPUS} CPUs, $((CPUS * MEM_PER_CPU / 1024))GB RAM"
    echo ""
    
    # Submit SELF
    SCRIPT_PATH="$(cd "$(dirname "$0")" && pwd)/$(basename "$0")"
    sbatch -n 1 --cpus-per-task=$CPUS \
        --mem-per-cpu=$MEM_PER_CPU \
        --time=$TIME \
        --job-name="gen_cfg_${SCENARIO_NAME}_i${ITERATION_NUMBER}" \
        --output="${LOG_DIR}/gen_config_${SCENARIO_NAME}_iter${ITERATION_NUMBER}_%j.out" \
        --error="${LOG_DIR}/gen_config_${SCENARIO_NAME}_iter${ITERATION_NUMBER}_%j.err" \
        "$SCRIPT_PATH" "$SCENARIO_NAME" "$ITERATION_NUMBER" "--internal-run"
        
    echo "✅ Job submitted!"
    echo "   Monitor: squeue --me"
    echo "   Logs:    ls -lh ${LOG_DIR}/gen_config_${SCENARIO_NAME}_iter${ITERATION_NUMBER}_*.out"
    exit 0
fi

# ================= PHASE 2: EXECUTION =================

echo "========================================="
if [ "$MODE" == "internal" ]; then
    echo "⚙️  RUNNING ON CLUSTER NODE"
    module load gcc/14.2.0
    module load java/jdk-21.0.6
    export MAVEN_HOME="$HOME/maven"
    export PATH="$MAVEN_HOME/bin:$PATH"
    
    # Try to load jq module, but don't fail if not found
    module load jq 2>/dev/null || true
    
    # Check if jq is actually available
    if ! command -v jq &> /dev/null; then
        echo "❌ Error: jq command not found (required for JSON parsing)"
        exit 1
    fi
    
    CLASSPATH="$JAR_FILE"
else
    echo "⚙️  RUNNING LOCALLY"
    
    # Check if jq is installed locally
    if ! command -v jq &> /dev/null; then
        echo "❌ Error: jq not installed locally"
        echo "   Install with: sudo apt-get install jq (Ubuntu) or brew install jq (macOS)"
        exit 1
    fi
    
    mvn dependency:build-classpath -Dmdep.outputFile=cp.txt > /dev/null 2>&1
    CLASSPATH=$(cat cp.txt):target/classes
fi
echo "========================================="

# Time interval mapping
declare -A time_map=(
    ["00-04h"]="0-4" ["04-08h"]="4-8" ["08-12h"]="8-12"
    ["12-16h"]="12-16" ["16-20h"]="16-20" ["20-24h"]="20-24"
)

# Helper Function: Merge vehicle files with unique time-based IDs
merge_vehicle_files_with_unique_ids() {
    local output_file=$1
    local zone=$2
    shift 2
    local input_files=("$@")
    
    echo '<!DOCTYPE vehicles SYSTEM "http://matsim.org/files/dtd/dvrp_vehicles_v1.dtd">' > "$output_file"
    echo '' >> "$output_file"
    echo '<vehicles>' >> "$output_file"
    
    local interval_index=0
    local total_vehicles=0
    
    for file in "${input_files[@]}"; do
        echo "    Interval $interval_index: Processing $(basename "$file")..."
        
        if [ ! -f "$file" ]; then
            echo "      ⚠️  Template not found, skipping"
            ((interval_index++))
            continue
        fi
        
        # Count vehicles using grep
        local vehicle_count=0
        vehicle_count=$(grep -c '<vehicle ' "$file" 2>/dev/null || echo "0")
        
        if [ "$vehicle_count" -eq 0 ]; then
            echo "      ⚠️  Empty file (0 vehicles), skipping"
            ((interval_index++))
            continue
        fi
        
        echo "      ✓ Merging $vehicle_count vehicles"
        
        # Extract and transform vehicle entries
        grep '<vehicle ' "$file" | \
            sed "s/id=\"drt_fleet_${zone}_/id=\"drt_fleet_${zone}_t${interval_index}_/g" >> "$output_file"
        
        total_vehicles=$((total_vehicles + vehicle_count))
        
        # Always increment at the end
        ((interval_index++))
    done
    
    echo '</vehicles>' >> "$output_file"
    echo "  ✓ Total vehicles merged: $total_vehicles"
}

# --- MAIN PROCESSING LOOP ---
echo "🚀 Processing: $SCENARIO_NAME (iteration $ITERATION_NUMBER)"
echo "   Input:  $JSON_INPUT_DIR"
echo "   Output: $BASE_OUTPUT_DIR"
echo ""

# Count JSON files to process
shopt -s nullglob  # Make glob return empty array if no matches
JSON_FILES=("$JSON_INPUT_DIR"/combined_solution_*_drt.json)
JSON_COUNT=${#JSON_FILES[@]}
echo "Found $JSON_COUNT solution JSON files to process"
echo ""

PROCESSED_COUNT=0
FAILED_COUNT=0

for json_file in "${JSON_FILES[@]}"; do
    scenario_id=$(basename "$json_file" _drt.json)
    SOLUTION_OUTPUT_DIR="$BASE_OUTPUT_DIR/$scenario_id"
    mkdir -p "$SOLUTION_OUTPUT_DIR" 
    
    echo "---------------------------------------------------"
    echo "Processing: $scenario_id"
    
    # 1. Extract Fleet Sizes from JSON
    for region in ne nw; do
        for interval in "00-04h" "04-08h" "08-12h" "12-16h" "16-20h" "20-24h"; do
            var_name="${region}_${interval//-/_}"
            var_name="${var_name//h/}"  # e.g., ne_00_04
            
            # Use jq to extract fleet size
            val=$(jq -r ".drt_solutions.drt_${region}.fleet_deployment.\"${interval}\".fleet_size" "$json_file")
            
            # Replace 0 with 25 (minimum fleet size)
            if [ "$val" -eq 0 ]; then val=25; fi
            
            # Assign to dynamic variable
            eval "$var_name=$val"
        done
    done
    
    echo "  NE fleets: $ne_00_04, $ne_04_08, $ne_08_12, $ne_12_16, $ne_16_20, $ne_20_24"
    echo "  NW fleets: $nw_00_04, $nw_04_08, $nw_08_12, $nw_12_16, $nw_16_20, $nw_20_24"

    # 2. Build Vehicle File Paths
    ne_files=()
    nw_files=()
    
    for interval in "00-04h" "04-08h" "08-12h" "12-16h" "16-20h" "20-24h"; do
        suffix="${time_map[$interval]}"
        
        # Get NE fleet size and build path
        var_name="ne_${interval//-/_}"; var_name="${var_name//h/}"
        ne_files+=("$DRT_VEHICLES_DIR/ne/drt_fleet_${!var_name}_${suffix}.xml")
        
        # Get NW fleet size and build path
        var_name="nw_${interval//-/_}"; var_name="${var_name//h/}"
        nw_files+=("$DRT_VEHICLES_DIR/nw/drt_fleet_${!var_name}_${suffix}.xml")
    done

    # 3. Merge Vehicle Files
    merged_ne="$SOLUTION_OUTPUT_DIR/drt_fleet_ne_merged.xml"
    merged_nw="$SOLUTION_OUTPUT_DIR/drt_fleet_nw_merged.xml"
    
    echo "  Merging NE vehicles..."
    merge_vehicle_files_with_unique_ids "$merged_ne" "ne" "${ne_files[@]}"
    
    echo "  Merging NW vehicles..."
    merge_vehicle_files_with_unique_ids "$merged_nw" "nw" "${nw_files[@]}"

    # 4. Generate MATSim Configs using Java
    # Convert to absolute paths for Java
    merged_ne_abs=$(cd "$(dirname "$merged_ne")" && pwd)/$(basename "$merged_ne")
    merged_nw_abs=$(cd "$(dirname "$merged_nw")" && pwd)/$(basename "$merged_nw")
    
    temp_cfg="$SOLUTION_OUTPUT_DIR/temp_input_config.xml"
    drt_cfg="$SOLUTION_OUTPUT_DIR/config_simulation_drt.xml"
    feeder_cfg="$SOLUTION_OUTPUT_DIR/config_simulation_drt_feeder.xml"
    
    cp "$TEMPLATE_CONFIG" "$temp_cfg"
    
    # Get absolute paths for Java arguments
    temp_cfg_abs=$(cd "$(dirname "$temp_cfg")" && pwd)/$(basename "$temp_cfg")
    drt_cfg_abs=$(cd "$(dirname "$drt_cfg")" && pwd)/$(basename "$drt_cfg")
    feeder_cfg_abs=$(cd "$(dirname "$feeder_cfg")" && pwd)/$(basename "$feeder_cfg")

    # Get absolute path to DRT shapefiles from matsim-leeds root
    NW_SHAPEFILE=$(readlink -f "data/supply/drt/nw_cluster_08_00_11_00.shp")
    NE_SHAPEFILE=$(readlink -f "data/supply/drt/ne_cluster_08_00_11_00.shp")

    echo "  Generating configs..."
    
    # Attempt Step 1: Generate DRT Config
    if java -cp "$CLASSPATH" com.husseinmahfouz.matsim.drt.RunAdaptConfigForDrt \
        --input-config-path "$temp_cfg_abs" \
        --output-config-path "$drt_cfg_abs" \
        --vehicles-paths "${merged_nw_abs},${merged_ne_abs}" \
        --mode-names "drtNW,drtNE" \
        --operational-schemes "serviceAreaBased,serviceAreaBased" \
        --add-leg-time-constraint "true,true" \
        --cost-models "LeedsDrtCostModel,LeedsDrtCostModel" \
        --estimators "LeedsDrtUtilityEstimator,LeedsDrtUtilityEstimator" \
        --mode-availability "LeedsDrtModeAvailability" \
        --config:multiModeDrt.drt[mode=drtNW].drtServiceAreaShapeFile="$NW_SHAPEFILE" \
        --config:multiModeDrt.drt[mode=drtNE].drtServiceAreaShapeFile="$NE_SHAPEFILE"; then
        
        # Step 1 Succeeded. Attempt Step 2: Feeder Config
        if java -cp "$CLASSPATH" com.husseinmahfouz.matsim.drt.RunAdaptConfigForFeederDrt \
            --input-config-path "$drt_cfg_abs" \
            --output-config-path "$feeder_cfg_abs" \
            --mode-names "drtNW_feeder,drtNE_feeder" \
            --base-pt-modes "pt" \
            --base-drt-modes "drtNW,drtNE" \
            --access-egress-transit-stop-modes "bus|rail,bus|rail" \
            --estimators "DefaultFeederDrtUtilityEstimator" \
            --mode-availability "FeederDrtModeAvailabilityWrapper"; then
            
            rm -f "$temp_cfg"
            echo "  ✅ Complete: $scenario_id"
            ((++PROCESSED_COUNT))
        else
            echo "  ❌ Failed at Feeder-DRT config generation"
            rm -f "$temp_cfg"
            ((++FAILED_COUNT))
        fi
    else
        echo "  ❌ Failed at DRT config generation"
        rm -f "$temp_cfg"
        ((++FAILED_COUNT))
    fi
done

echo ""
echo "========================================="
echo "✅ Config generation complete: $SCENARIO_NAME (iteration $ITERATION_NUMBER)"
echo "   Processed: $PROCESSED_COUNT / $JSON_COUNT solutions"
if [ $FAILED_COUNT -gt 0 ]; then
    echo "   ⚠️  Failed: $FAILED_COUNT solutions"
fi
echo "========================================="