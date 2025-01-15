#!/bin/bash

# Run this file from the following directory: matsim-leeds
# If file is in matsim-leeds/bash, run ./bash/CreateDrtVehicles.sh


# Define the fixed arguments
networkPath="data/supply/network_mapped.xml.gz"
randomSeed="1234"
networkModes="car"

# Define the different sets of arguments
declare -a argsArray=(
    "drt_fleet_1_ data/supply/drt/oa_leeds1.shp 100 4 0 86400 data/supply/drt/drt_fleet_1.xml"
    "drt_fleet_2_ data/supply/drt/oa_leeds2.shp 150 6 0 86400 data/supply/drt/drt_fleet_2.xml"
    "drt_fleet_3_ data/supply/drt/oa_leeds1.shp 200 8 0 86400 data/supply/drt/drt_fleet_3.xml"
)

# Get the classpath for all dependencies and append target/classes (the former has core functionality e.g. core eqasim, the latter has the classes I wrote and compile
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

