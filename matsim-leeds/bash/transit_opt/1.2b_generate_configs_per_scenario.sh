#!/bin/bash
# filepath: bash/transit_opt/1.2b_generate_configs_per_scenario.sh
# Run from matsim-leeds root: bash bash/transit_opt/1.2b_generate_scenario_configs.sh <scenario_name>


# This script combines the logic of AdaptConfigForDrt.sh and AdaptConfigForFeederDrt.sh, but is used
# to create dynamic fleets (fleets that change in size throughout the day) based on optimization results stored in JSON files.

# Steps:
    # 1a. Look at the drt json file of each solution, and get the corresponding drt fleet xml files
        # Each JSON file contains fleet deployment information for two DRT zones (NE and NW) across 6 time intervals.
    # 1b. Combine DRT fleet xml files per time period into one xml file
    # 2. Run AdaptConfigforDRT and AdaptConfigforFeederDRT to prepare MATSim config for simulation with feeder DRT 

if [ $# -ne 1 ]; then
    echo "Usage: $0 <scenario_name>"
    echo "Example: $0 scenario1"
    exit 1
fi

SCENARIO_NAME=$1

# Path to transit_opt repo
TRANSIT_OPT_REPO="../../transit_opt"

# Input directory (DRT JSON files in transit_opt repo)
JSON_INPUT_DIR="$TRANSIT_OPT_REPO/output/$SCENARIO_NAME"

# Base output directory
BASE_OUTPUT_DIR="data/supply/transit_opt_paper/$SCENARIO_NAME"

# DRT vehicles base directory
DRT_VEHICLES_DIR="data/supply/transit_opt_paper/drt_fleet_templates"

# Template config
TEMPLATE_CONFIG="src/main/resources/config_simulation_dmc.xml"

# Get the classpath
mvn dependency:build-classpath -Dmdep.outputFile=cp.txt
CLASSPATH=$(cat cp.txt):target/classes



# Time interval mapping
declare -A time_map=(
    ["00-04h"]="0-4"
    ["04-08h"]="4-8"
    ["08-12h"]="8-12"
    ["12-16h"]="12-16"
    ["16-20h"]="16-20"
    ["20-24h"]="20-24"
)

# Function to merge vehicle files with unique IDs
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
        if [ ! -f "$file" ]; then
            echo "    Interval $interval_index: File not found (skipping)"
            ((interval_index++))
            continue
        fi
        
        local vehicle_count=0
        if grep -q '<vehicle ' "$file" 2>/dev/null; then
            vehicle_count=$(grep '<vehicle ' "$file" 2>/dev/null | wc -l)
            vehicle_count=${vehicle_count##*([[:space:]])}
        fi
        
        if [ "$vehicle_count" -eq 0 ]; then
            echo "    Interval $interval_index: Skipping (0 vehicles)"
            ((interval_index++))
            continue
        fi
        
        echo "    Interval $interval_index: Found $vehicle_count vehicles"
        
        grep '<vehicle ' "$file" | \
            sed "s/id=\"drt_fleet_${zone}_/id=\"drt_fleet_${zone}_t${interval_index}_/g" >> "$output_file"
        
        total_vehicles=$((total_vehicles + vehicle_count))
        ((interval_index++))
    done
    
    echo '</vehicles>' >> "$output_file"
    echo "  ✓ Total vehicles merged: $total_vehicles"
}

echo "========================================="
echo "Generating configs for: $SCENARIO_NAME"
echo "========================================="

# Process each JSON file
for json_file in "$JSON_INPUT_DIR"/combined_solution_*_drt.json; do
    [ -f "$json_file" ] || continue
    
    scenario_id=$(basename "$json_file" _drt.json)
    
    # Create output directory for this solution
    SOLUTION_OUTPUT_DIR="$BASE_OUTPUT_DIR/$scenario_id"
    mkdir -p "$SOLUTION_OUTPUT_DIR" 
    
    echo ""
    echo "Processing: $scenario_id"
    
    # Extract fleet sizes
    ne_00_04=$(jq -r '.drt_solutions.drt_ne.fleet_deployment."00-04h".fleet_size' "$json_file")
    ne_04_08=$(jq -r '.drt_solutions.drt_ne.fleet_deployment."04-08h".fleet_size' "$json_file")
    ne_08_12=$(jq -r '.drt_solutions.drt_ne.fleet_deployment."08-12h".fleet_size' "$json_file")
    ne_12_16=$(jq -r '.drt_solutions.drt_ne.fleet_deployment."12-16h".fleet_size' "$json_file")
    ne_16_20=$(jq -r '.drt_solutions.drt_ne.fleet_deployment."16-20h".fleet_size' "$json_file")
    ne_20_24=$(jq -r '.drt_solutions.drt_ne.fleet_deployment."20-24h".fleet_size' "$json_file")
    
    nw_00_04=$(jq -r '.drt_solutions.drt_nw.fleet_deployment."00-04h".fleet_size' "$json_file")
    nw_04_08=$(jq -r '.drt_solutions.drt_nw.fleet_deployment."04-08h".fleet_size' "$json_file")
    nw_08_12=$(jq -r '.drt_solutions.drt_nw.fleet_deployment."08-12h".fleet_size' "$json_file")
    nw_12_16=$(jq -r '.drt_solutions.drt_nw.fleet_deployment."12-16h".fleet_size' "$json_file")
    nw_16_20=$(jq -r '.drt_solutions.drt_nw.fleet_deployment."16-20h".fleet_size' "$json_file")
    nw_20_24=$(jq -r '.drt_solutions.drt_nw.fleet_deployment."20-24h".fleet_size' "$json_file")
    
    # Replace 0 with 10 (comment out to disable)
    for var in ne_00_04 ne_04_08 ne_08_12 ne_12_16 ne_16_20 ne_20_24 nw_00_04 nw_04_08 nw_08_12 nw_12_16 nw_16_20 nw_20_24; do
        [ "${!var}" -eq 0 ] && eval "$var=10"
    done
    
    echo "NE fleet sizes: $ne_00_04, $ne_04_08, $ne_08_12, $ne_12_16, $ne_16_20, $ne_20_24"
    echo "NW fleet sizes: $nw_00_04, $nw_04_08, $nw_08_12, $nw_12_16, $nw_16_20, $nw_20_24"
    
    # Build vehicle file arrays
    ne_files=()
    for interval in "00-04h" "04-08h" "08-12h" "12-16h" "16-20h" "20-24h"; do
        var_name="ne_${interval//-/_}"
        var_name="${var_name//h/}"
        fleet_size="${!var_name}"
        time_suffix="${time_map[$interval]}"
        ne_files+=("$DRT_VEHICLES_DIR/ne/drt_fleet_${fleet_size}_${time_suffix}.xml")
    done
    
    nw_files=()
    for interval in "00-04h" "04-08h" "08-12h" "12-16h" "16-20h" "20-24h"; do
        var_name="nw_${interval//-/_}"
        var_name="${var_name//h/}"
        fleet_size="${!var_name}"
        time_suffix="${time_map[$interval]}"
        nw_files+=("$DRT_VEHICLES_DIR/nw/drt_fleet_${fleet_size}_${time_suffix}.xml")
    done
    
    # Merge vehicles
    merged_ne_file="$SOLUTION_OUTPUT_DIR/drt_fleet_ne_merged.xml"
    merged_nw_file="$SOLUTION_OUTPUT_DIR/drt_fleet_nw_merged.xml"
    
    echo "Merging NE vehicles..."
    merge_vehicle_files_with_unique_ids "$merged_ne_file" "ne" "${ne_files[@]}"
    
    echo "Merging NW vehicles..."
    merge_vehicle_files_with_unique_ids "$merged_nw_file" "nw" "${nw_files[@]}"
    
    # Convert to absolute paths
    merged_ne_file_abs=$(cd "$(dirname "$merged_ne_file")" && pwd)/$(basename "$merged_ne_file")
    merged_nw_file_abs=$(cd "$(dirname "$merged_nw_file")" && pwd)/$(basename "$merged_nw_file")
    all_vehicles_paths_abs="${merged_nw_file_abs},${merged_ne_file_abs}"
    
    # Config paths (now in solution directory)
    temp_input_config="$SOLUTION_OUTPUT_DIR/temp_input_config.xml"
    drt_config="$SOLUTION_OUTPUT_DIR/config_simulation_drt.xml"
    feeder_config="$SOLUTION_OUTPUT_DIR/config_simulation_drt_feeder.xml"
    
    cp "$TEMPLATE_CONFIG" "$temp_input_config"
    
    temp_input_config_abs=$(cd "$(dirname "$temp_input_config")" && pwd)/$(basename "$temp_input_config")
    drt_config_abs=$(cd "$(dirname "$drt_config")" && pwd)/$(basename "$drt_config")
    feeder_config_abs=$(cd "$(dirname "$feeder_config")" && pwd)/$(basename "$feeder_config")
    
    echo "Generating DRT config..."
    java -cp $CLASSPATH com.husseinmahfouz.matsim.drt.RunAdaptConfigForDrt \
        --input-config-path "$temp_input_config_abs" \
        --output-config-path "$drt_config_abs" \
        --vehicles-paths "$all_vehicles_paths_abs" \
        --mode-names "drtNW,drtNE" \
        --operational-schemes "serviceAreaBased,serviceAreaBased" \
        --add-leg-time-constraint "true,true" \
        --cost-models "LeedsDrtCostModel,LeedsDrtCostModel" \
        --estimators "LeedsDrtUtilityEstimator,LeedsDrtUtilityEstimator" \
        --mode-availability "LeedsDrtModeAvailability" \
        --config:multiModeDrt.drt[mode=drtNW].drtServiceAreaShapeFile=data/supply/drt/nw_cluster_08_00_11_00.shp \
        --config:multiModeDrt.drt[mode=drtNE].drtServiceAreaShapeFile=data/supply/drt/ne_cluster_08_00_11_00.shp
    
    if [ $? -ne 0 ]; then
        echo "❌ Error creating DRT config for $scenario_id"
        rm -f "$temp_input_config"
        continue
    fi
    
    echo "Generating Feeder config..."
    java -cp $CLASSPATH com.husseinmahfouz.matsim.drt.RunAdaptConfigForFeederDrt \
        --input-config-path "$drt_config_abs" \
        --output-config-path "$feeder_config_abs" \
        --mode-names "drtNW_feeder,drtNE_feeder" \
        --base-pt-modes "pt" \
        --base-drt-modes "drtNW,drtNE" \
        --access-egress-transit-stop-modes "bus|rail,bus|rail" \
        --estimators "DefaultFeederDrtUtilityEstimator" \
        --mode-availability "FeederDrtModeAvailabilityWrapper"
    
    if [ $? -ne 0 ]; then
        echo "❌ Error creating Feeder config for $scenario_id"
        rm -f "$temp_input_config"
        continue
    fi
    
    rm -f "$temp_input_config"
    echo "  ✓ Complete: $scenario_id"
done

echo ""
echo "========================================="
echo "All configs generated for $SCENARIO_NAME"
echo "========================================="
