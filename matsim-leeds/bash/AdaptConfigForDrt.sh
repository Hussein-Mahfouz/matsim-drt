#!/bin/bash

# This script is used to adapt a config to work with the eqasim drt extension. I have written it so that it can create multiple config files in case I want to have different scenarios


# Run this file from the following directory: matsim-leeds
# If file is in matsim-leeds/bash, run ./bash/AdaptConfigForDrt.sh

# Define the fixed arguments
inputConfigPath="src/main/resources/config_simulation_dmc.xml"
outputConfigPath="src/main/resources/config_simulation_dmc_drt.xml"

# Define the different sets of arguments
declare -a argsArray=(
    "data/supply/drt/drt_fleet_1.xml,data/supply/drt/drt_fleet_2.xml drtA,drtB door2door,serviceAreaBased true,true"
   #"data/supply/drt/drt_fleet_1.xml,data/supply/drt/drt_fleet_2.xml door2door,serviceAreaBased true,false"

)

# Get the classpath for all dependencies and append target/classes (the former has core functionality e.g. core eqasim, the latter has the classes I wrote and compile
mvn dependency:build-classpath -Dmdep.outputFile=cp.txt
CLASSPATH=$(cat cp.txt):target/classes

# Loop through the arguments and run the Java program
for args in "${argsArray[@]}"; do
    set -- $args
    vehiclesPaths=$1
    modeNames=$2
    operationalSchemes=$3
    addLegTimeConstraint=$4

    java -cp $CLASSPATH com.husseinmahfouz.matsim.drt.RunAdaptConfigForDrt \
        --input-config-path $inputConfigPath \
        --output-config-path $outputConfigPath \
        --vehicles-paths $vehiclesPaths \
        --mode-names $modeNames \
        --operational-schemes $operationalSchemes \
        --add-leg-time-constraint $addLegTimeConstraint # only set to false if you want to check demand out of the specified fleet operating times. You will have a lot of rejections
done

