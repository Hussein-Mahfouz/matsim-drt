#!/bin/bash

# Run this file from the following directory: matsim-leeds
# If file is in matsim-leeds/bash, run ./bash/CreateDrtVehicles.sh


# Define the fixed arguments
networkPath="data/supply/network_mapped.xml.gz"
randomSeed="1234"
networkModes="car"

# Define the different sets of arguments
declare -a argsArray=(
    "drt_fleet_1 data/supply/drt/pt_wkday_06_30_scenario_3 100 4 0 86400 data/supply/drt/drt_fleet_1.xml"
    "drt_fleet_2 data/supply/drt/pt_wkday_06_30_scenario_3.shp 150 6 0 86400 data/supply/drt/drt_fleet_2.xml"
    "drt_fleet_3 data/supply/drt/pt_wkday_06_30_scenario_3.shp 200 8 0 86400 data/supply/drt/drt_fleet_3.xml"
)

# Get the classpath for all dependencies and append target/classes
mvn dependency:build-classpath -Dmdep.outputFile=cp.txt
CLASSPATH=$(cat cp.txt):target/classes

# Loop through the arguments and run the Java program
for args in "${argsArray[@]}"; do
    set -- $args
    vehicleIdPrefix=$1
    serviceAreaPath=$2
    vehiclesNumber=$3
    vehiclesCapacity=$4
    serviceBeginTime=$5
    serviceEndTime=$6
    outputVehiclesPath=$7

    java -cp $CLASSPATH com.husseinmahfouz.matsim.drt.RunCreateDrtVehicles \
        --network-path $networkPath \
        --output-vehicles-path $outputVehiclesPath \
        --vehicles-number $vehiclesNumber \
        --vehicles-capacity $vehiclesCapacity \
        --service-begin-time $serviceBeginTime \
        --service-end-time $serviceEndTime \
        --random-seed $randomSeed \
        --vehicle-id-prefix $vehicleIdPrefix \
        --service-area-path $serviceAreaPath \
        --network-modes $networkModes
done

