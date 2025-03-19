#!/bin/bash

# This script is used to adapt a config to work with the eqasim drt extension. I have written it so that it can create multiple config files in case I want to have different scenarios

# Run this file from the following directory: matsim-leeds
# If file is in matsim-leeds/bash, run ./bash/AdaptConfigForDrt.sh

# only set addLegTimeConstraint to false if you want to check demand out of the specified fleet operating times. You will have a lot of rejections

# Define the fixed arguments
inputConfigPath="src/main/resources/config_simulation_dmc.xml"
outputConfigPath="src/main/resources/config_simulation_dmc_drt.xml"
# outputConfigPath="src/main/resources/config_simulation_dmc_drt_all.xml"

# Define the different sets of arguments
declare -a argsArray=(
    # Config that adds two fleets for the northwest and northeast service areas
    "data/supply/drt/nw/fleets/drt_fleet_nw_50.xml,data/supply/drt/ne/fleets/drt_fleet_ne_50.xml \
    drtNW,drtNE \
    serviceAreaBased,serviceAreaBased \
    true,true \
    LeedsDrtCostModel,LeedsDrtCostModel \
    LeedsDrtUtilityEstimator,LeedsDrtUtilityEstimator \
    LeedsDrtModeAvailability"
)

# declare -a argsArray=(
#      # --- Config that adds a fleet for the entire study area
#     "data/supply/drt/all/fleets/drt_fleet_all_50.xml \
#     drt \
#     door2door \
#     true
#     LeedsDrtCostModel \
#     LeedsDrtUtilityEstimator \
#     LeedsDrtModeAvailability"
# )

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