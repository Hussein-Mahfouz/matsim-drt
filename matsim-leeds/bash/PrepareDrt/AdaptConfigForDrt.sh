#!/bin/bash

# This script is used to adapt a config to work with the eqasim drt extension. I have written it so that it can create multiple config files in case I want to have different scenarios

# Run this file from the following directory: matsim-leeds
# If file is in matsim-leeds/bash, run ./bash/AdaptConfigForDrt.sh

# only set addLegTimeConstraint to false if you want to check demand out of the specified fleet operating times. You will have a lot of rejections

# Define the list of fleet sizes
fleet_sizes=("50" "100" "200" "500" "1000")

# Loop through each fleet size
for FLEET_SIZE in "${fleet_sizes[@]}"; do
    # Define the fixed arguments
    inputConfigPath="src/main/resources/fleet_sizing/config_simulation_dmc.xml"

    # --- Define the different sets of arguments
    outputConfigPath="src/main/resources/fleet_sizing/config_simulation_dmc_drt_${FLEET_SIZE}.xml"
    declare -a argsArray=(
        # Config that adds two fleets for the northwest and northeast service areas
        "data/supply/drt/nw/fleets/drt_fleet_nw_${FLEET_SIZE}.xml,data/supply/drt/ne/fleets/drt_fleet_ne_${FLEET_SIZE}.xml \
        drtNW,drtNE \
        serviceAreaBased,serviceAreaBased \
        ../../../../data/supply/drt/nw_cluster_08_00_11_00.shp,../../../../data/supply/drt/ne_cluster_08_00_11_00.shp \
        true,true \
        LeedsDrtCostModel,LeedsDrtCostModel \
        LeedsDrtUtilityEstimator,LeedsDrtUtilityEstimator \
        LeedsDrtModeAvailability"
    )

    # Get the classpath for all dependencies and append target/classes (the former has core functionality e.g. core eqasim, the latter has the classes I wrote and compile)
    mvn dependency:build-classpath -Dmdep.outputFile=cp.txt
    CLASSPATH=$(cat cp.txt):target/classes

    # Loop through the arguments and run the Java program
    for args in "${argsArray[@]}"; do
        set -- $args
        vehiclesPaths=$1
        modeNames=$2
        operationalSchemes=$3
        serviceAreaPaths=$4
        addLegTimeConstraint=$5
        drtCostModel=$6
        drtUtilityEstimator=$7
        modeAvailability=$8

        # Split the service area paths into two separate paths
        IFS=',' read -r serviceAreaPathNW serviceAreaPathNE <<< "$serviceAreaPaths"

        java -cp $CLASSPATH com.husseinmahfouz.matsim.drt.RunAdaptConfigForDrt \
            --input-config-path $inputConfigPath \
            --output-config-path $outputConfigPath \
            --vehicles-paths $vehiclesPaths \
            --mode-names $modeNames \
            --operational-schemes $operationalSchemes \
            --add-leg-time-constraint $addLegTimeConstraint \
            --cost-models $drtCostModel \
            --estimators $drtUtilityEstimator \
            --mode-availability $modeAvailability \
            --config:multiModeDrt.drt[mode=drtNW].drtServiceAreaShapeFile=$serviceAreaPathNW \
            --config:multiModeDrt.drt[mode=drtNE].drtServiceAreaShapeFile=$serviceAreaPathNE
    done
done


# ----------- Same but without service areas! Entire study area

for FLEET_SIZE in "${fleet_sizes[@]}"; do
    # Define the fixed arguments
    inputConfigPath="src/main/resources/fleet_sizing/config_simulation_dmc.xml"


    # --- Define the different sets of arguments
    outputConfigPath="src/main/resources/fleet_sizing/config_simulation_dmc_drt_all_${FLEET_SIZE}.xml"
    declare -a argsArray=(
        # Config that adds a fleet for the entire study area
        "data/supply/drt/all/fleets/drt_fleet_all_${FLEET_SIZE}.xml \
        drt \
        door2door \
        true \
        LeedsDrtCostModel \
        LeedsDrtUtilityEstimator \
        LeedsDrtModeAvailability"
    )

    # Get the classpath for all dependencies and append target/classes (the former has core functionality e.g. core eqasim, the latter has the classes I wrote and compile)
    mvn dependency:build-classpath -Dmdep.outputFile=cp.txt
    CLASSPATH=$(cat cp.txt):target/classes

    # Loop through the arguments and run the Java program
    for args in "${argsArray[@]}"; do
        set -- $args
        vehiclesPaths=$1
        modeNames=$2
        operationalSchemes=$3
        addLegTimeConstraint=$4
        drtCostModel=$5
        drtUtilityEstimator=$6
        modeAvailability=$7

        java -cp $CLASSPATH com.husseinmahfouz.matsim.drt.RunAdaptConfigForDrt \
            --input-config-path $inputConfigPath \
            --output-config-path $outputConfigPath \
            --vehicles-paths $vehiclesPaths \
            --mode-names $modeNames \
            --operational-schemes $operationalSchemes \
            --add-leg-time-constraint $addLegTimeConstraint \
            --cost-models $drtCostModel \
            --estimators $drtUtilityEstimator \
            --mode-availability $modeAvailability 
    done
done
