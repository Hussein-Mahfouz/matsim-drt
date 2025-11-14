#!/bin/bash

# RUN LOCALLY. THIS IS MEANT TO TEST OUT WORKFLOWS SO I CAN THEN TAKE THE CODE TO 2_run_scenarios_cluster.sh
set -e

# Ensure the required software is installed and configured on your laptop
# For example, you might need to set JAVA_HOME or MAVEN_HOME if not already set

# Get the current working directory (assuming you run this script from the matsim-leeds directory)
MATSIM_DIR="$(pwd)"   # This automatically sets the current directory to MATSIM_DIR

# Define the paths to the JAR file and the config file
JAR_FILE="$MATSIM_DIR/target/matsim-leeds-1.0.jar"
CONFIG_FILE="$MATSIM_DIR/src/main/resources/config_simulation_dmc_drt_50_feeder.xml"

# Define the fully qualified name of the main class (RunDMCSimulation)
MAIN_CLASS="com.husseinmahfouz.matsim.RunDMCSimulationDRT"

# Define the population sample size being used
SAMPLE_SIZE="0.02" 
USE_REJECTION_CONSTRAINT="true"
ITERATIONS=5
GLOBAL_THREADS=8
QSIM_THREADS=8

# Shared input files (based on sample size)
INPUT_PLANS_FILE="$MATSIM_DIR/data/demand/plans_sample_eqasim_${SAMPLE_SIZE}.xml"
VEHICLES_FILE="$MATSIM_DIR/data/supply/network_vehicles_${SAMPLE_SIZE}.xml"
TRANSIT_VEHICLES_FILE="$MATSIM_DIR/data/supply/vehicles_unmapped.xml"
TRANSIT_SCHEDULE_FILE="$MATSIM_DIR/data/supply/schedule_mapped.xml.gz"
NETWORK_INPUT_FILE="$MATSIM_DIR/data/supply/network_mapped.xml.gz"
OUTPUT_DIRECTORY="$MATSIM_DIR/scenarios/test/results_dmc_waiting"

# Define the population sample size being used
SAMPLE_SIZE="0.03" 
USE_REJECTION_CONSTRAINT="true"
ITERATIONS=15
GLOBAL_THREADS=8
QSIM_THREADS=8

# DRT parameters (to control rejection rate) See issue #55
ENABLE_REJECTION_PENALTY="true"  # Set to "false" to disable
TARGET_REJECTION_RATE="0.05"  # 5% target
CONTROLLER_GAIN="3.0"         # Proportional gain

# Run the simulation directly
java -Xmx48G -cp $JAR_FILE $MAIN_CLASS \
    --config-path $CONFIG_FILE \
    --sample-size $SAMPLE_SIZE \
    --use-rejection-constraint $USE_REJECTION_CONSTRAINT \
    --iterations $ITERATIONS \
    --global-threads $GLOBAL_THREADS \
    --qsim-threads $QSIM_THREADS \
    --input-plans-file $INPUT_PLANS_FILE \
    --vehicles-file $VEHICLES_FILE \
    --transit-vehicles-file $TRANSIT_VEHICLES_FILE \
    --transit-schedule-file $TRANSIT_SCHEDULE_FILE \
    --network-input-file $NETWORK_INPUT_FILE \
    --output-directory $OUTPUT_DIRECTORY \
    --enable-rejection-penalty $ENABLE_REJECTION_PENALTY \
    --target-rejection-rate $TARGET_REJECTION_RATE \
    --controller-gain $CONTROLLER_GAIN