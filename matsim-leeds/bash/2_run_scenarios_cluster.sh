#!/bin/bash

# RUN FROM CLUSTER 
set -e

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
JAR_FILE="$MATSIM_DIR/matsim-leeds-1.0.jar"
CONFIG_FILE="$MATSIM_DIR/src/main/resources/config_simulation_dmc_drt_feeder_3pct_cluster.xml"

# Define the fully qualified name of the main class (RunDMCSimulation) - to get path, use: jar tf target/matsim-leeds-1.0.jar | grep "RunDMCSimulation"
MAIN_CLASS="com.husseinmahfouz.matsim.RunDMCSimulationDRTCluster"

# Define the compute-related parameters
CPUS_PER_TASK=12
MEM_PER_CPU=8192
MAX_RUNTIME="4:00:00"
GLOBAL_THREADS=12
QSIM_THREADS=12
# Should not be higher than CPUS_PER_TASK
GLOBAL_THREADS=12
# Should not be higher than CPUS_PER_TASK
QSIM_THREADS=12

# Define the population sample size being used
SAMPLE_SIZE="0.01" 
# Define whether to use the rejection constraint
USE_REJECTION_CONSTRAINT="true"
# Define the output directory
OUTPUT_DIRECTORY="scenarios/sample_${SAMPLE_SIZE}"
# Define the input plans file
INPUT_PLANS_FILE="data/demand/plans_sample_eqasim_${SAMPLE_SIZE}.xml"
# Define the vehicles file (it differs based on the population sample - see NetworkVehicleInserter.java)
VEHICLES_FILE="data/supply/network_vehicles_${SAMPLE_SIZE}.xml"


# Run the first simulation using sbatch
JOB_ID_1=$(sbatch -n 1 --cpus-per-task=$CPUS_PER_TASK --time=$MAX_RUNTIME --mem-per-cpu=$MEM_PER_CPU --wrap="\
    java -Xmx48G -cp $JAR_FILE $MAIN_CLASS \
    --config-path $CONFIG_FILE \
    --sample-size $SAMPLE_SIZE \
    --use-rejection-constraint $USE_REJECTION_CONSTRAINT \
    --output-directory $OUTPUT_DIRECTORY \
    --input-plans-file $INPUT_PLANS_FILE \
    --vehicles-file $VEHICLES_FILE \
    --global-threads $GLOBAL_THREADS \
    --qsim-threads $QSIM_THREADS" | awk '{print $4}')
echo "Submitted job $JOB_ID_1 for sample size $SAMPLE_SIZE"

# Define parameters for the second run
SAMPLE_SIZE_2="0.05"
OUTPUT_DIRECTORY_2="scenarios/sample_${SAMPLE_SIZE_2}"
INPUT_PLANS_FILE_2="data/demand/plans_sample_eqasim_${SAMPLE_SIZE_2}.xml"
VEHICLES_FILE_2="data/supply/network_vehicles_${SAMPLE_SIZE_2}.xml"

# Run the second simulation using sbatch
JOB_ID_2=$(sbatch -n 1 --cpus-per-task=$CPUS_PER_TASK --time=$MAX_RUNTIME --mem-per-cpu=$MEM_PER_CPU --wrap="\
    java -Xmx48G -cp $JAR_FILE $MAIN_CLASS \
    --config-path $CONFIG_FILE \
    --sample-size $SAMPLE_SIZE_2 \
    --use-rejection-constraint $USE_REJECTION_CONSTRAINT \
    --output-directory $OUTPUT_DIRECTORY_2 \
    --input-plans-file $INPUT_PLANS_FILE_2 \
    --vehicles-file $VEHICLES_FILE_2 \
    --global-threads $GLOBAL_THREADS \
    --qsim-threads $QSIM_THREADS" | awk '{print $4}')
echo "Submitted job $JOB_ID_2 for sample size $SAMPLE_SIZE_2"

# Define parameters for the third run
SAMPLE_SIZE_3="0.20"
OUTPUT_DIRECTORY_3="scenarios/sample_${SAMPLE_SIZE_3}"
INPUT_PLANS_FILE_3="data/demand/plans_sample_eqasim_${SAMPLE_SIZE_3}.xml"
VEHICLES_FILE_3="data/supply/network_vehicles_${SAMPLE_SIZE_3}.xml"

# Run the third simulation using sbatch
JOB_ID_3=$(sbatch -n 1 --cpus-per-task=$CPUS_PER_TASK --time=$MAX_RUNTIME --mem-per-cpu=$MEM_PER_CPU --wrap="\
    java -Xmx48G -cp $JAR_FILE $MAIN_CLASS \
    --config-path $CONFIG_FILE \
    --sample-size $SAMPLE_SIZE_3 \
    --use-rejection-constraint $USE_REJECTION_CONSTRAINT \
    --output-directory $OUTPUT_DIRECTORY_3 \
    --input-plans-file $INPUT_PLANS_FILE_3 \
    --vehicles-file $VEHICLES_FILE_3 \
    --global-threads $GLOBAL_THREADS \
    --qsim-threads $QSIM_THREADS" | awk '{print $4}')
echo "Submitted job $JOB_ID_3 for sample size $SAMPLE_SIZE_3"