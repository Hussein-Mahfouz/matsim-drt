#!/bin/bash

# RUN LOCALLY. THIS IS MEANT TO TEST OUT WORKFLOWS SO I CAN THEN TAKE THE CODE TO 2_run_scenarios_cluster.sh
set -e

# Ensure the required software is installed and configured on your laptop
# For example, you might need to set JAVA_HOME or MAVEN_HOME if not already set

# Get the current working directory (assuming you run this script from the matsim-leeds directory)
MATSIM_DIR="$(pwd)"   # This automatically sets the current directory to MATSIM_DIR

# Define the paths to the JAR file and the config file
JAR_FILE="$MATSIM_DIR/matsim-leeds-1.0.jar"
CONFIG_FILE="$MATSIM_DIR/src/main/resources/config_simulation_dmc_drt_feeder_3pct.xml"

# Define the population sample size being used
SAMPLE_SIZE="0.03" 

# Define the fully qualified name of the main class (RunDMCSimulation)
MAIN_CLASS="com.husseinmahfouz.matsim.dmc.RunDMCSimulationDRT"

# Run the simulation directly
java -Xmx48G -cp $JAR_FILE $MAIN_CLASS --config-path $CONFIG_FILE --sample-size $SAMPLE_SIZE
