#!/bin/bash

# RUN FROM CLUSTER 
set -e

# This script is meant to run multiple scenarios, each with a different fleet size
# The configs and drt vehicle files are prepared in advance using AdaptConfigforDRT 
# and AdaptConfigForFeederDrt.sh 

# The plans and vehicles files are prepared in advance using 0_create_pop_sample.sh

# --- modules that worked for ETH cluster

# module load stack/2024-06
# module load gcc/12.2.0
# module load maven/3.8.4
# module load openjdk/21.0.6

# --- Leeds HPC has different modules

module load gcc/14.2.0
module load java/jdk-21.0.6
# Load local Maven instead of module (Maven is not on Aire, so I installed locally)
export MAVEN_HOME="$HOME/maven"
export PATH="$MAVEN_HOME/bin:$PATH"

# Get the current working directory (assuming you run this script from the matsim-leeds directory)
MATSIM_DIR="$(pwd)"   # This automatically sets the current directory to MATSIM_DIR

# Define the paths to the JAR file and the config file
JAR_FILE="$MATSIM_DIR/target/matsim-leeds-1.0.jar"

# Define the fully qualified name of the main class (RunDMCSimulation) - to get path, use: jar tf target/matsim-leeds-1.0.jar | grep "RunDMCSimulation"
MAIN_CLASS="com.husseinmahfouz.matsim.RunDMCSimulationDRTCluster"

# Define the compute-related parameters
CPUS_PER_TASK=12
MEM_PER_CPU=8192
MAX_RUNTIME="8:00:00"
# Should not be higher than CPUS_PER_TASK
GLOBAL_THREADS=12
# Should not be higher than CPUS_PER_TASK
QSIM_THREADS=12
# Number of iterations
ITERATIONS=75

# Define the population sample size being used (plans and vehicles files need to exist for this sample size)
SAMPLE_SIZE="1.00" # 0.50, 0.20, 0.10, 0.05, 0.01
# Define whether to use the rejection constraint
USE_REJECTION_CONSTRAINT="true"

# Define the list of configuration files relative to MATSIM_DIR
config_files=(
    "config_simulation_dmc_drt_50_feeder.xml"
    "config_simulation_dmc_drt_all_50_feeder.xml"
    "config_simulation_dmc_drt_100_feeder.xml"
    "config_simulation_dmc_drt_all_100_feeder.xml"
    "config_simulation_dmc_drt_200_feeder.xml"
    "config_simulation_dmc_drt_all_200_feeder.xml"
    "config_simulation_dmc_drt_500_feeder.xml"
    "config_simulation_dmc_drt_all_500_feeder.xml"
    "config_simulation_dmc_drt_1000_feeder.xml"
    "config_simulation_dmc_drt_all_1000_feeder.xml"
)

# Loop through each configuration file and submit a job
for CONFIG_FILE in "${config_files[@]}"; do
    # Prepend MATSIM_DIR to the config file path
    FULL_CONFIG_PATH="$MATSIM_DIR/src/main/resources/fleet_sizing/$CONFIG_FILE"

    # Extract the fleet size and whether it is an "all" configuration or not
    if [[ $CONFIG_FILE == *"_all_"* ]]; then
        CONFIG_TYPE="all"
    else
        CONFIG_TYPE="zones"
    fi
    FLEET_SIZE=$(echo $CONFIG_FILE | grep -oP '\d+')

    # Define the output directory based on the configuration file name
    OUTPUT_DIRECTORY="scenarios/fleet_sizing/${CONFIG_TYPE}/${FLEET_SIZE}/sample_${SAMPLE_SIZE}"
    # Define the input plans file
    INPUT_PLANS_FILE="../../../../data/demand/plans_sample_eqasim_${SAMPLE_SIZE}.xml"
    # Define the vehicles file (it differs based on the population sample - see NetworkVehicleInserter.java)
    VEHICLES_FILE="../../../../data/supply/network_vehicles_${SAMPLE_SIZE}.xml"

    # Submit the job using sbatch
    JOB_ID=$(sbatch -n 1 --cpus-per-task=$CPUS_PER_TASK --time=$MAX_RUNTIME --mem-per-cpu=$MEM_PER_CPU --wrap="\
        java -Xmx48G -cp $JAR_FILE $MAIN_CLASS \
        --config-path $FULL_CONFIG_PATH \
        --global-threads $GLOBAL_THREADS \
        --qsim-threads $QSIM_THREADS \
        --iterations $ITERATIONS \
        --sample-size $SAMPLE_SIZE \
        --use-rejection-constraint $USE_REJECTION_CONSTRAINT \
        --output-directory $OUTPUT_DIRECTORY \
        --input-plans-file $INPUT_PLANS_FILE \
        --vehicles-file $VEHICLES_FILE" | awk '{print $4}')
    echo "Submitted job $JOB_ID for config file $CONFIG_FILE"

done