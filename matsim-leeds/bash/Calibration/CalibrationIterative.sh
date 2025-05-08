#!/bin/bash
set -e

# Run this file from the following directory: matsim-leeds
# If file is in matsim-leeds/bash, run "bash bash/Calibration/CalibrationIterative.sh"

# This script is used to iteratively calibrate the MATSim simulation for Leeds using the DMC extension.
# --- Step 1: 
# Define the (a) reference mode shares (REF_<MODE>) and the (b) initial ASC values (ASC_<MODE>). 
# Th ASC values are from the existing choice model in LeedsModeParameters.
# --- Step 2:
# Define the convergence thresholds for each mode (THRESHOLD_<MODE>). If actual mode share is 
# within the threshold of the reference mode share, we consider it converged.
# --- Step 3:
# Define the adjustment step for the ASCs at each iteration (STEP). 
# This is the amount by which we adjust the ASC of each mode if the mode share is outside the threshold.
# --- Step 4:
# Define the maximum number of iterations (MAX_ITER). Each iteration is a full simulation run (with its own number of iterations).
# Between each simulation run, the ASCs are adjusted based on the mode shares.
# The process can finish early if convergence is achieved before reaching the maximum number of iterations.
# --- Other parmeters to define:
# Number of iterations WITHIN each simulation run (ITERATIONS),
# Sample size (SAMPLE_SIZE), (You need to have the input plans file and vehicles file for the sample size)
# Input plans file (INPUT_PLANS_FILE) and vehicles file (VEHICLES_FILE) for the sample size
# parent directory for all scenarios (PARENT_DIRECTORY),
# --- Output
# The script will create a centralized CSV file for storing mode shares (MODE_SHARE_CSV) and
# a centralized CSV file for storing ASCs (ASC_CSV) in the parent directory for all scenarios (PARENT_DIRECTORY).



# Define reference mode shares
REF_CAR=0.55  # car-passenger is fixed at 25%
REF_PT=0.07
REF_BIKE=0.02
REF_WALK=0.08
REF_TAXI=0.03

# Define initial ASC values
ASC_CAR=0.0  # Fixed, will not change
ASC_BUS=-0.0929
ASC_RAIL=2.4421
ASC_BIKE=-4.0728
ASC_WALK=3.0294
ASC_TAXI=-1.8075

# Define convergence thresholds for each mode
THRESHOLD_CAR=0.01
THRESHOLD_PT=0.01
THRESHOLD_BIKE=0.01
THRESHOLD_WALK=0.01
THRESHOLD_TAXI=0.01

# Get the current working directory (assuming you run this script from the matsim-leeds directory)
MATSIM_DIR="$(pwd)"   # This automatically sets the current directory to MATSIM_DIR

# Define the adjustment step for the ASCs at each iteration
STEP=0.03 #0.05

# Define maximum iterations. Each iterations is a full simulation run (with it's own number of iterations). 
# Between each simulation run, the ASCs are adjusted based on the mode shares.
MAX_ITER=10

# Path to configuration file
CONFIG_PATH="$MATSIM_DIR/src/main/resources/calibration/config_simulation_dmc_calibration.xml"

# Path to the JAR file
JAR_FILE="$MATSIM_DIR/target/matsim-leeds-1.0.jar"
MAIN_CLASS="com.husseinmahfouz.matsim.dmc.calibration.RunDMCSimulationCalibration"

# Define the sample size
SAMPLE_SIZE="0.05"  
# Define the number of iterations WITHIN each simulation run
ITERATIONS=10

# Define the input plans file
INPUT_PLANS_FILE="../../../../data/demand/plans_sample_eqasim_${SAMPLE_SIZE}.xml"
# Define the vehicles file (it differs based on the population sample - see NetworkVehicleInserter.java)
VEHICLES_FILE="../../../../data/supply/network_vehicles_${SAMPLE_SIZE}.xml"


# Define the parent directory for all scenarios
PARENT_DIRECTORY="$MATSIM_DIR/scenarios/calibration"
mkdir -p $PARENT_DIRECTORY

# Create a centralized CSV file for storing mode shares
MODE_SHARE_CSV="$PARENT_DIRECTORY/mode_shares.csv"
echo "Iteration,Car,PT,Bike,Walk,Taxi" > "$MODE_SHARE_CSV"

# Create a centralized CSV file for storing ASCs
ASC_CSV="$PARENT_DIRECTORY/asc_values.csv"
echo "Iteration,ASC_CAR,ASC_BUS,ASC_RAIL,ASC_BIKE,ASC_WALK,ASC_TAXI" > "$ASC_CSV"


# Iterative calibration loop
for ((i=1; i<=MAX_ITER; i++)); do
    echo "Iteration $i"

    # Save the current ASCs to the CSV file
    echo "$i,$ASC_CAR,$ASC_BUS,$ASC_RAIL,$ASC_BIKE,$ASC_WALK,$ASC_TAXI" >> "$ASC_CSV"


    # Define a unique output directory for this iteration
    OUTPUT_DIRECTORY="$PARENT_DIRECTORY/iteration_$i"
    mkdir -p $OUTPUT_DIRECTORY

    # Update MODESTATS_PATH dynamically
    MODESTATS_PATH="$OUTPUT_DIRECTORY/modestats.csv"

    # Run the simulation
    java -Xmx32g -cp $JAR_FILE $MAIN_CLASS \
        --config-path $CONFIG_PATH \
        --sample-size $SAMPLE_SIZE \
        --iterations $ITERATIONS \
        --output-directory "$OUTPUT_DIRECTORY" \
        --mode-choice-parameter:car.alpha_u=$ASC_CAR \
        --mode-choice-parameter:leedsPT.alpha_u_Bus=$ASC_BUS \
        --mode-choice-parameter:leedsPT.alpha_u_Rail=$ASC_RAIL \
        --mode-choice-parameter:leedsTaxi.alpha_u=$ASC_TAXI \
        --mode-choice-parameter:bike.alpha_u=$ASC_BIKE \
        --mode-choice-parameter:walk.alpha_u=$ASC_WALK

    # Check if the simulation ran successfully
    if [ $? -ne 0 ]; then
        echo "Simulation failed in iteration $i."
        exit 1
    fi

    # Check if modestats.csv exists
    if [ ! -f "$MODESTATS_PATH" ]; then
        echo "modestats.csv not found in $OUTPUT_DIRECTORY. Ensure the simulation generates this file."
        exit 1
    fi

    # Extract the last row of modestats.csv (to get mode share at the end of simulation)
    HEADER=$(head -n 1 "$MODESTATS_PATH")
    LAST_ROW=$(tail -n 1 "$MODESTATS_PATH")

    # Function to get the value of a specific column by name (Used to get mode share of each mode from modestats.csv)
    get_column_value() {
        local column_name=$1
        local header=$2
        local row=$3

        # Find the column index for the specified column name
        local column_index=$(echo "$header" | awk -v col="$column_name" -F';' '{for (i=1; i<=NF; i++) if ($i == col) print i}')

        # Extract the value for the specified column index
        echo "$row" | awk -v idx="$column_index" -F';' '{print $idx}'
}

    # Extract mode shares by matching column names
    MODE_SHARE_CAR=$(get_column_value "car" "$HEADER" "$LAST_ROW")
    MODE_SHARE_PT=$(get_column_value "pt" "$HEADER" "$LAST_ROW")
    MODE_SHARE_BIKE=$(get_column_value "bike" "$HEADER" "$LAST_ROW")
    MODE_SHARE_WALK=$(get_column_value "walk" "$HEADER" "$LAST_ROW")
    MODE_SHARE_TAXI=$(get_column_value "taxi" "$HEADER" "$LAST_ROW")

    # Append mode shares to the centralized CSV file
    echo "$i,$MODE_SHARE_CAR,$MODE_SHARE_PT,$MODE_SHARE_BIKE,$MODE_SHARE_WALK,$MODE_SHARE_TAXI" >> "$MODE_SHARE_CSV"

    # Adjust ASCs based on comparison to reference (car ASC is fixed)
    if [ "$(echo "$MODE_SHARE_PT > $REF_PT + $THRESHOLD_PT" | bc -l)" -eq 1 ]; then
        ASC_BUS=$(echo "$ASC_BUS - $STEP" | bc)
    elif [ "$(echo "$MODE_SHARE_PT < $REF_PT - $THRESHOLD_PT" | bc -l)" -eq 1 ]; then
        ASC_BUS=$(echo "$ASC_BUS + $STEP" | bc)
    fi

    if [ "$(echo "$MODE_SHARE_BIKE > $REF_BIKE + $THRESHOLD_BIKE" | bc -l)" -eq 1 ]; then
        ASC_BIKE=$(echo "$ASC_BIKE - $STEP" | bc)
    elif [ "$(echo "$MODE_SHARE_BIKE < $REF_BIKE - $THRESHOLD_BIKE" | bc -l)" -eq 1 ]; then
        ASC_BIKE=$(echo "$ASC_BIKE + $STEP" | bc)
    fi

    if [ "$(echo "$MODE_SHARE_WALK > $REF_WALK + $THRESHOLD_WALK" | bc -l)" -eq 1 ]; then
        ASC_WALK=$(echo "$ASC_WALK - $STEP" | bc)
    elif [ "$(echo "$MODE_SHARE_WALK < $REF_WALK - $THRESHOLD_WALK" | bc -l)" -eq 1 ]; then
        ASC_WALK=$(echo "$ASC_WALK + $STEP" | bc)
    fi

    if [ "$(echo "$MODE_SHARE_TAXI > $REF_TAXI + $THRESHOLD_TAXI" | bc -l)" -eq 1 ]; then
        ASC_TAXI=$(echo "$ASC_TAXI - $STEP" | bc)
    elif [ "$(echo "$MODE_SHARE_TAXI < $REF_TAXI - $THRESHOLD_TAXI" | bc -l)" -eq 1 ]; then
        ASC_TAXI=$(echo "$ASC_TAXI + $STEP" | bc)
    fi

    # Check for convergence
    CONVERGED=true
    if (( $(echo "($MODE_SHARE_CAR - $REF_CAR)^2 > $THRESHOLD_CAR^2" | bc -l) )); then CONVERGED=false; fi
    if (( $(echo "($MODE_SHARE_PT - $REF_PT)^2 > $THRESHOLD_PT^2" | bc -l) )); then CONVERGED=false; fi
    if (( $(echo "($MODE_SHARE_BIKE - $REF_BIKE)^2 > $THRESHOLD_BIKE^2" | bc -l) )); then CONVERGED=false; fi
    if (( $(echo "($MODE_SHARE_WALK - $REF_WALK)^2 > $THRESHOLD_WALK^2" | bc -l) )); then CONVERGED=false; fi
    if (( $(echo "($MODE_SHARE_TAXI - $REF_TAXI)^2 > $THRESHOLD_TAXI^2" | bc -l) )); then CONVERGED=false; fi

    if $CONVERGED; then
        echo "Convergence achieved after $i iterations."
        break
    fi
done

if ! $CONVERGED; then
    echo "Calibration did not converge after $MAX_ITER iterations."
fi