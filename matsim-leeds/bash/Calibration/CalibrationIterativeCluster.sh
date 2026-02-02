#!/bin/bash

# Run this file from the following directory: matsim-leeds
# If file is in matsim-leeds/bash, run "sbatch bash/Calibration/CalibrationIterative.sh"

# This script is used to iteratively calibrate the MATSim simulation for Leeds using the DMC extension.
# --- Step 1: 
# Define the (a) reference mode shares (REF_<MODE>) and the (b) initial ASC values (ASC_<MODE>). 
# The ASC values are from the existing choice model in LeedsModeParameters.
# --- Step 2:
# Define the convergence thresholds for each mode (THRESHOLD_<MODE>). In this log-ratio 
# approach, thresholds serve as the exit criteria for the loop; however, mathematical 
# adjustments continue for all modes until the entire system converges.
# --- Step 3:
# Define the adjustment 'learning rate' (STEP). 
# We use a log-ratio update: ASC_new = ASC_old + STEP * ln(Reference_Share / Simulated_Share).
# This provides a proportional adjustment where large errors result in larger shifts, 
# and small errors result in fine-tuned "nudges" for higher stability.
# --- Step 4:
# Define the maximum number of iterations (MAX_ITER). Each iteration is a full simulation run.
# Between each simulation run, the ASCs are adjusted based on the mode shares extracted 
# from the modestats.csv file.
# The process can finish early if convergence is achieved before reaching the maximum number of iterations.
# --- Other parameters to define:
# Number of iterations WITHIN each simulation run (ITERATIONS),
# Sample size (SAMPLE_SIZE),
# Input plans file (INPUT_PLANS_FILE) and vehicles file (VEHICLES_FILE) for the sample size,
# parent directory for all scenarios (PARENT_DIRECTORY).
# --- Output
# The script will create a centralized CSV file for storing mode shares (MODE_SHARE_CSV) and
# a centralized CSV file for storing ASCs (ASC_CSV) in the parent directory for all scenarios (PARENT_DIRECTORY).

#!/bin/bash
#SBATCH --job-name=calibration        # Name of the job
#SBATCH --output=slurm-%j.out         # Standard output log (%j = Job ID)
#SBATCH --error=slurm-%j.err          # Standard error log
#SBATCH --time=36:00:00               # Wall time limit (hh:mm:ss)
#SBATCH --ntasks=1                    # Number of tasks (one process)
#SBATCH --cpus-per-task=16            # Number of CPU cores per task
#SBATCH --mem-per-cpu=8192            # Memory per CPU core in MB (8192 MB = 8 GB)

# ================================
# Load necessary modules
# ================================
module load gcc/14.2.0               # Load GCC version 14.2.0
module load java/jdk-21.0.6         # Load Java JDK 21.0.6

# Load local Maven (installed manually)
export MAVEN_HOME="$HOME/maven"
export PATH="$MAVEN_HOME/bin:$PATH"


# ================================
# Compute-related parameters
# ================================
CPUS_PER_TASK=${SLURM_CPUS_PER_TASK:-12}  # Total CPUs assigned per task. Default to 12 if not set
MEM_PER_CPU=${SLURM_MEM_PER_CPU:-8192}   # (in MB) Default to 8192 MB if not set
JAVA_MEMORY=$(echo "0.9 * $CPUS_PER_TASK * $MEM_PER_CPU" | bc | awk '{printf "%.0f", $1}')m # Java memory allocation (90% of total memory)
MAX_RUNTIME="36:00:00" # Maximum runtime for the job (hh:mm:ss)

# MATSim-specific thread settings

GLOBAL_THREADS=$CPUS_PER_TASK # For global MATSim tasks (should not exceed CPUS_PER_TASK)
QSIM_THREADS=$CPUS_PER_TASK  # For queue simulation threads (should not exceed CPUS_PER_TASK)

# ================================
# File paths and parameters
# ================================

# Define the sample size
SAMPLE_SIZE="0.25"  

# Get the current working directory (assuming you run this script from the matsim-leeds directory)
MATSIM_DIR="$(pwd)"   # This automatically sets the current directory to MATSIM_DIR

# Path to the JAR file
JAR_FILE="$MATSIM_DIR/target/matsim-leeds-1.0.jar"
MAIN_CLASS="com.husseinmahfouz.matsim.dmc.calibration.RunDMCSimulationCalibration"

# Define the parent directory for all scenarios
PARENT_DIRECTORY="$MATSIM_DIR/scenarios/calibration_${SAMPLE_SIZE}"
mkdir -p $PARENT_DIRECTORY

# Define the input plans file
INPUT_PLANS_FILE="../../../../data/demand/plans_sample_eqasim_${SAMPLE_SIZE}.xml"
# Define the vehicles file (it differs based on the population sample - see NetworkVehicleInserter.java)
VEHICLES_FILE="../../../../data/supply/network_vehicles_${SAMPLE_SIZE}.xml"


# ================================
# Calibration-related parameters
# ================================


# Define reference mode shares (from: https://s3-eu-west-2.amazonaws.com/commonplace-customer-assets/leedstransportstrategy/Leeds%20Transport%20Strategy_p11.pdf)
# See also https://committees.parliament.uk/writtenevidence/106339/pdf/
REF_CAR=0.38  # car-passenger is fixed at 23%
REF_PT=0.10 # bus: 9% rail:1%
REF_BIKE=0.01
REF_WALK=0.25
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


# Define a damping factor (STEP). For ASC adjustment (Logit-based adjustment)
# 1.0 = Full log adjustment. 0.5 to 0.8 is usually safer to prevent oscillation.
STEP=0.5

# Define maximum iterations. Each iterations is a full simulation run (with it's own number of iterations). 
# Between each simulation run, the ASCs are adjusted based on the mode shares.
MAX_ITER=75

# Create a centralized CSV file for storing mode shares
MODE_SHARE_CSV="$PARENT_DIRECTORY/mode_shares.csv"
echo "Iteration,Car,PT,Bike,Walk,Taxi" > "$MODE_SHARE_CSV"

# Create a centralized CSV file for storing ASCs
ASC_CSV="$PARENT_DIRECTORY/asc_values.csv"
echo "Iteration,ASC_CAR,ASC_BUS,ASC_RAIL,ASC_BIKE,ASC_WALK,ASC_TAXI" > "$ASC_CSV"

# ================================
# Individual simulation parameters
# ================================

# Path to configuration file
CONFIG_PATH="$MATSIM_DIR/src/main/resources/calibration/config_simulation_dmc_calibration.xml"

# Define the number of iterations WITHIN each simulation run
ITERATIONS=60

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
    java -Xmx$JAVA_MEMORY -cp $JAR_FILE $MAIN_CLASS \
        --config-path $CONFIG_PATH \
        --global-threads $GLOBAL_THREADS \
        --qsim-threads $QSIM_THREADS \
        --sample-size $SAMPLE_SIZE \
        --iterations $ITERATIONS \
        --output-directory $OUTPUT_DIRECTORY \
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

    # --- LOG-BASED ADJUSTMENT LOGIC ---
    # Formula: ASC_new = ASC_old + (STEP * ln(Target_Share / Simulated_Share))
    # 'l()' is the natural log function in the 'bc -l' library.
    # We use scale=10 to ensure the log function l() doesn't round to 0

    # Adjust PT (Updating BUS based on PT share, keeping RAIL fixed as per original)
    ASC_BUS=$(echo "scale=7; $ASC_BUS + ($STEP * l($REF_PT / $MODE_SHARE_PT))" | bc -l)

    # Adjust Bike
    ASC_BIKE=$(echo "scale=7; $ASC_BIKE + ($STEP * l($REF_BIKE / $MODE_SHARE_BIKE))" | bc -l)

    # Adjust Walk
    ASC_WALK=$(echo "scale=7; $ASC_WALK + ($STEP * l($REF_WALK / $MODE_SHARE_WALK))" | bc -l)

    # Adjust Taxi
    ASC_TAXI=$(echo "scale=7; $ASC_TAXI + ($STEP * l($REF_TAXI / $MODE_SHARE_TAXI))" | bc -l)

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
    # Add a delay before the next iteration (in seconds)
    sleep 10
done

if ! $CONVERGED; then
    echo "Calibration did not converge after $MAX_ITER iterations."
fi