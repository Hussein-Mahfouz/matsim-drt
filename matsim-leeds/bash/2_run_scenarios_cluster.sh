#!/bin/bash

# RUN FROM CLUSTER 
set -e

module load stack/2024-06
module load gcc/12.2.0
module load maven/3.8.4
module load openjdk/21.0.3_9


# Get the current working directory (assuming you run this script from the matsim-leeds directory)
MATSIM_DIR="$(pwd)"   # This automatically sets the current directory to MATSIM_DIR

# Define the paths to the JAR file and the config file
JAR_FILE="$MATSIM_DIR/matsim-leeds-1.0.jar"
CONFIG_FILE="$MATSIM_DIR/src/main/resources/config_simulation_dmc_drt_feeder_3pct.xml"

# Define the population sample size being used
SAMPLE_SIZE="0.03" 


# Define the fully qualified name of the main class (RunDMCSimulation) - to get path, use: jar tf target/matsim-leeds-1.0.jar | grep "RunDMCSimulation"
MAIN_CLASS="com.husseinmahfouz.matsim.dmc.drt.RunDMCSimulationDRT"

# Run the simulation using sbatch
sbatch -n 1 --cpus-per-task=12 --time=2:00:00 --mem-per-cpu=4096 \
    --wrap="java -Xmx48G -cp $JAR_FILE $MAIN_CLASS --config-path $CONFIG_FILE --sample-size $SAMPLE_SIZE"

