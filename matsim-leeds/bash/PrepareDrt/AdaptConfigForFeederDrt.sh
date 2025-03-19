#!/bin/bash

# This script is used to adapt a config to work with the eqasim feeder drt extension. I have written it so that it can create multiple config files in case I want to have different scenarios.

# Run this file from the following directory: matsim-leeds
# If file is in matsim-leeds/bash, run ./bash/AdaptConfigForFeederDrt.sh

# Define the fixed arguments


# Define the different sets of arguments (each row will produce a different config, so the outputCOnfigPath should be unique
declare -a argsArray=(
    "src/main/resources/config_simulation_dmc_drt.xml src/main/resources/config_simulation_dmc_drt_feeder.xml drtA_feeder,drtB_feeder pt drtA,drtB bus|rail,bus LeedsDrtUtilityEstimator LeedsDrtModeAvailability"
    # "src/main/resources/config_simulation_dmc_drt.xml src/main/resources/config_simulation_dmc_drt_feeder2.xml drtA_feeder,drtB_feeder pt drtA rail,bus LeedsDrtUtilityEstimator LeedsDrtModeAvailability"
)

# Get the classpath for all dependencies and append target/classes (the former has core functionality e.g. core eqasim, the latter has the classes I wrote and compiled)
mvn dependency:build-classpath -Dmdep.outputFile=cp.txt
CLASSPATH=$(cat cp.txt):target/classes

# Loop through the arguments and run the Java program
for args in "${argsArray[@]}"; do
    set -- $args
    inputConfigPath=$1
    outputConfigPath=$2
    modeNames=$3
    basePtModes=$4 # can remove as "pt" is default
    baseDrtModes=$5
    accessEgressTransitStopModes=$6
   # accessEgressTransitStopIDs=$
    estimators=$7
    modeAvailability=$8

    java -cp $CLASSPATH org.eqasim.core.simulation.modes.feeder_drt.utils.AdaptConfigForFeederDrt \
        --input-config-path $inputConfigPath \
        --output-config-path $outputConfigPath \
        --mode-names $modeNames \
        --base-pt-modes $basePtModes \
        --base-drt-modes $baseDrtModes \
        --access-egress-transit-stop-modes $accessEgressTransitStopModes \
        --estimators $estimators \
        --mode-availability $modeAvailability
done
