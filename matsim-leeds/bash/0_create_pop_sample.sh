#!/bin/bash

# RUN FROM LOCAL MACHINE
set -e

# To run from matsim-leeds/ directory, use: bash bash/0_create_pop_sample.sh

# Get the current working directory (assuming you run this script from the matsim-leeds directory)
MATSIM_DIR="$(pwd)"

# Define the sample size
SAMPLE_SIZE="1.00"

# Define the input and output files for CreatePopulationSample
INPUT_POPULATION_FILE="$MATSIM_DIR/data/demand/plans.xml"
OUTPUT_POPULATION_FILE="$MATSIM_DIR/data/demand/plans_sample_${SAMPLE_SIZE}.xml"

# Define the input and output files for NetworkVehicleInserter
CONFIG_FILE="$MATSIM_DIR/src/main/resources/config_simulation_dmc.xml"
OUTPUT_VEHICLES_FILE="$MATSIM_DIR/data/supply/network_vehicles_${SAMPLE_SIZE}.xml"
FINAL_OUTPUT_POPULATION_FILE="$MATSIM_DIR/data/demand/plans_sample_eqasim_${SAMPLE_SIZE}.xml"

# Define the path to the JAR file
JAR_FILE="$MATSIM_DIR/target/matsim-leeds-1.0.jar"

# Run CreatePopulationSample
echo "Running CreatePopulationSample with sample size $SAMPLE_SIZE..."
java -cp "$JAR_FILE" com.husseinmahfouz.matsim.sample.CreatePopulationSample \
    --input $INPUT_POPULATION_FILE --output $OUTPUT_POPULATION_FILE --sample $SAMPLE_SIZE

# Run NetworkVehicleInserter
echo "Running NetworkVehicleInserter with sample size $SAMPLE_SIZE..."
java -cp "$JAR_FILE" com.husseinmahfouz.matsim.dmc.NetworkVehicleInserter \
    $CONFIG_FILE $OUTPUT_POPULATION_FILE $OUTPUT_VEHICLES_FILE $FINAL_OUTPUT_POPULATION_FILE

echo "Scripts executed successfully!"