#!/bin/bash
# filepath: bash/PrepareDrt/GenerateScenarioConfigs.sh

# This script combines the logic of AdaptConfigForDrt.sh and AdaptConfigForFeederDrt.sh, but is used
# to create dynamic fleets (fleets that change in size throughout the day) based on optimization results stored in JSON files.

# Generate MATSim configs for multiple DRT fleet scenarios from JSON files.
# Each JSON file contains fleet deployment information for two DRT zones (NE and NW) across 6 time intervals.
# Run from matsim-leeds root: bash/PrepareDrt/GenerateScenarioConfigs.sh

# Get the classpath
mvn dependency:build-classpath -Dmdep.outputFile=cp.txt
CLASSPATH=$(cat cp.txt):target/classes

# Template config (without DRT)
TEMPLATE_CONFIG="src/main/resources/config_simulation_dmc.xml"

# Directory containing JSON scenario files
SCENARIOS_DIR="data/external/gtfs_optimisation/test"

# Output directory for generated configs
OUTPUT_CONFIG_DIR="data/external/gtfs_optimisation/test"
mkdir -p "$OUTPUT_CONFIG_DIR"

# Directory for merged vehicle files
MERGED_VEHICLES_DIR="$OUTPUT_CONFIG_DIR/merged_vehicles"
mkdir -p "$MERGED_VEHICLES_DIR"

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
        
        # Count vehicles in this file - use subshell to avoid set -e issues
        local vehicle_count=0
        if grep -q '<vehicle ' "$file" 2>/dev/null; then
            vehicle_count=$(grep '<vehicle ' "$file" 2>/dev/null | wc -l)
            vehicle_count=${vehicle_count##*([[:space:]])}  # trim whitespace
        fi
        
        # Skip files with 0 vehicles
        if [ "$vehicle_count" -eq 0 ]; then
            echo "    Interval $interval_index: Skipping (0 vehicles)"
            ((interval_index++))
            continue
        fi
        
        echo "    Interval $interval_index: Found $vehicle_count vehicles"
        
        # Extract and rename vehicles
        grep '<vehicle ' "$file" | \
            sed "s/id=\"drt_fleet_${zone}_/id=\"drt_fleet_${zone}_t${interval_index}_/g" >> "$output_file"
        
        total_vehicles=$((total_vehicles + vehicle_count))
        ((interval_index++))
    done
    
    echo '</vehicles>' >> "$output_file"
    echo "  ✓ Total vehicles merged: $total_vehicles"
}

# Process each JSON file
for json_file in "$SCENARIOS_DIR"/combined_solution_*.json; do
    [ -f "$json_file" ] || continue
    
    scenario_id=$(basename "$json_file" .json)
    echo ""
    echo "========================================="
    echo "Processing scenario: $scenario_id"
    echo "========================================="
    
    # Extract fleet sizes for NE and NW zones
    echo "Extracting fleet sizes from JSON..."
    
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
    
    # Replace 0 with 10 for all fleet sizes (comment out to disable)
    for var in ne_00_04 ne_04_08 ne_08_12 ne_12_16 ne_16_20 ne_20_24 nw_00_04 nw_04_08 nw_08_12 nw_12_16 nw_16_20 nw_20_24; do
        [ "${!var}" -eq 0 ] && eval "$var=10"
    done
    
    # Print extracted fleet sizes
    echo "NE fleet sizes: $ne_00_04, $ne_04_08, $ne_08_12, $ne_12_16, $ne_16_20, $ne_20_24"
    echo "NW fleet sizes: $nw_00_04, $nw_04_08, $nw_08_12, $nw_12_16, $nw_16_20, $nw_20_24"
    
    # Check if all fleet sizes are 0
    total_fleet=$((ne_00_04 + ne_04_08 + ne_08_12 + ne_12_16 + ne_16_20 + ne_20_24 + \
                   nw_00_04 + nw_04_08 + nw_08_12 + nw_12_16 + nw_16_20 + nw_20_24))
    
    if [ "$total_fleet" -eq 0 ]; then
        echo "⚠️  WARNING: All fleet sizes are 0 for this scenario. Skipping config generation."
        continue
    fi
    
    # Build arrays of vehicle file paths
    ne_files=()
    for interval in "00-04h" "04-08h" "08-12h" "12-16h" "16-20h" "20-24h"; do
        var_name="ne_${interval//-/_}"
        var_name="${var_name//h/}"
        fleet_size="${!var_name}"
        time_suffix="${time_map[$interval]}"
        ne_files+=("data/supply/drt/ne/fleets/temporal/drt_fleet_${fleet_size}_${time_suffix}.xml")
    done
    
    nw_files=()
    for interval in "00-04h" "04-08h" "08-12h" "12-16h" "16-20h" "20-24h"; do
        var_name="nw_${interval//-/_}"
        var_name="${var_name//h/}"
        fleet_size="${!var_name}"
        time_suffix="${time_map[$interval]}"
        nw_files+=("data/supply/drt/nw/fleets/temporal/drt_fleet_${fleet_size}_${time_suffix}.xml")
    done
    
    # Merge vehicle files for each zone with unique IDs
    merged_ne_file="$MERGED_VEHICLES_DIR/${scenario_id}_drt_fleet_ne_merged.xml"
    merged_nw_file="$MERGED_VEHICLES_DIR/${scenario_id}_drt_fleet_nw_merged.xml"
    
    echo ""
    echo "Merging NE vehicle files..."
    merge_vehicle_files_with_unique_ids "$merged_ne_file" "ne" "${ne_files[@]}"
    
    echo ""
    echo "Merging NW vehicle files..."
    merge_vehicle_files_with_unique_ids "$merged_nw_file" "nw" "${nw_files[@]}"
    
    # Combine both zones' merged vehicle paths
    all_vehicles_paths="${merged_nw_file},${merged_ne_file}"
    
    # Output config paths
    drt_config="$OUTPUT_CONFIG_DIR/${scenario_id}_config_drt.xml"
    feeder_config="$OUTPUT_CONFIG_DIR/${scenario_id}_config_feeder.xml"
    
        echo ""
    echo "Generating configs..."

    # Convert relative paths to absolute paths
    merged_ne_file_abs=$(cd "$(dirname "$merged_ne_file")" && pwd)/$(basename "$merged_ne_file")
    merged_nw_file_abs=$(cd "$(dirname "$merged_nw_file")" && pwd)/$(basename "$merged_nw_file")
    all_vehicles_paths_abs="${merged_nw_file_abs},${merged_ne_file_abs}"

    # Copy template config to output directory
    temp_input_config="$OUTPUT_CONFIG_DIR/temp_input_config.xml"
    cp "$TEMPLATE_CONFIG" "$temp_input_config"
    
    # Convert config paths to absolute paths
    temp_input_config_abs=$(cd "$(dirname "$temp_input_config")" && pwd)/$(basename "$temp_input_config")
    drt_config_abs="$(cd "$(dirname "$drt_config")" && pwd)/$(basename "$drt_config")"
    feeder_config_abs="$(cd "$(dirname "$feeder_config")" && pwd)/$(basename "$feeder_config")"
    

    echo "Using absolute paths:"
    echo "  Template config (copied): $temp_input_config_abs"
    echo "  DRT config: $drt_config_abs"
    echo "  Feeder config: $feeder_config_abs"
    echo "  NW vehicles: $merged_nw_file_abs"
    echo "  NE vehicles: $merged_ne_file_abs"
    
    
    
    # Step 1: Adapt config for DRT
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
        rm -f "$temp_input_config"  # Clean up temp file
        continue
    fi
    
    echo "  ✓ DRT config created: $drt_config_abs"
    
    # Step 2: Adapt config for Feeder DRT
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
        echo "❌ Error creating Feeder DRT config for $scenario_id"
        rm -f "$temp_input_config"  # Clean up temp file
        continue
    fi
    
    echo "  ✓ Feeder config created: $feeder_config_abs"
    
    # Clean up temporary config
    # rm -f "$temp_input_config"
done

echo ""
echo "========================================="
echo "All scenario configs processed"
echo "========================================="