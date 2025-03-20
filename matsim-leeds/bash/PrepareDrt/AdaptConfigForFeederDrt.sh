#!/bin/bash

# This script is used to adapt a config to work with the eqasim feeder drt extension. I have written it so that it can create multiple config files in case I want to have different scenarios.

# Run this file from the following directory: matsim-leeds
# If file is in matsim-leeds/bash, run ./bash/AdaptConfigForFeederDrt.sh

# Define the list of fleet sizes
fleet_sizes=("50" "100" "200" "500" "1000")

# Loop through each fleet size
for FLEET_SIZE in "${fleet_sizes[@]}"; do

    # # --- SERVICE AREA BASED: Define the different sets of arguments

    inputConfigPath="src/main/resources/fleet_sizing/config_simulation_dmc_drt_${FLEET_SIZE}.xml"
    outputConfigPath="src/main/resources/fleet_sizing/config_simulation_dmc_drt_${FLEET_SIZE}_feeder.xml"
    declare -a argsArray=(
        # Config that adds two fleets for the northwest and northeast service areas
        "drtNW_feeder,drtNE_feeder \
        pt \
        drtNW,drtNE \
        bus|rail,bus|rail \
        LeedsDrtUtilityEstimator \
        FeederDrtModeAvailabilityWrapper"
    )

    # # --- ENTIRE STUDY AREA (door2door): Define the different sets of arguments

    # inputConfigPath="src/main/resources/fleet_sizing/config_simulation_dmc_drt_all_${FLEET_SIZE}.xml"
    # outputConfigPath="src/main/resources/fleet_sizing/config_simulation_dmc_drt_all_${FLEET_SIZE}_feeder.xml"
    # declare -a argsArray=(
    #     # Config that adds a fleet for the entire study area
    #     "drt_feeder \
    #     pt \
    #     drt \
    #     bus|rail \
    #     LeedsDrtUtilityEstimator \
    #     FeederDrtModeAvailabilityWrapper"
    # )

    # Get the classpath for all dependencies and append target/classes (the former has core functionality e.g. core eqasim, the latter has the classes I wrote and compiled)
    mvn dependency:build-classpath -Dmdep.outputFile=cp.txt
    CLASSPATH=$(cat cp.txt):target/classes

    # Loop through the arguments and run the Java program
    # Loop through the arguments and run the Java program
    for args in "${argsArray[@]}"; do
        set -- $args
        modeNames=$1
        basePtModes=$2 # can remove as "pt" is default
        baseDrtModes=$3
        accessEgressTransitStopModes=$4
        # accessEgressTransitStopIDs=$
        estimators=$5
        modeAvailability=$6

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
done