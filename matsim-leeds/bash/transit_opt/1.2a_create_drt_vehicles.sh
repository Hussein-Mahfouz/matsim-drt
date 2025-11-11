#!/bin/bash

# This script is meant to pregenerate DRT files for all possible time range and fleet size combinations
# We then use these templates in our scenario modelling. We extract the templates we need for
# each time period 

# how it works: It takes a DRT service area and creates a fleet of DRT vehicles for different time ranges.
# You can specify a different fleet size for each time range in the last argument of timeRanges.

# Run from matsim-leeds root: bash/transit_opt/1.2a_create_drt_vehicles.sh
# Change Service area configuration arguments as needed

# THIS SCRIPT ONLY NEEDS TO BE RUN ONCE IN THE BEGINNING

set -e

# Get the classpath for all dependencies and append target/classes
mvn dependency:build-classpath -Dmdep.outputFile=cp.txt
CLASSPATH=$(cat cp.txt):target/classes

# Fixed arguments
networkPath="data/supply/network_mapped.xml.gz"
randomSeed="1234"
networkModes="car"
vehiclesCapacity=4

# Service area configuration
serviceAreaPath="data/supply/drt/nw_cluster_08_00_11_00.shp"
vehicleIdPrefix="drt_fleet_nw_"
outputSubdirectory="nw"

# Check if the shapefile exists
if [ ! -f "$serviceAreaPath" ]; then
    echo "Shapefile not found: $serviceAreaPath"
    exit 1
fi

# Create output directory
mkdir -p "data/supply/transit_opt_paper/drt_fleet_templates/$outputSubdirectory/"

# Define fleet sizes
fleet_sizes=(0 10 25 50 100)

# Define time intervals (beginHour endHour serviceBeginTime serviceEndTime)
time_intervals=(
    "0 4 0 14400"
    "4 8 14400 28800"
    "8 12 28800 43200"
    "12 16 43200 57600"
    "16 20 57600 72000"
    "20 24 72000 86400"
)

# Generate vehicle files for all combinations
for fleet_size in "${fleet_sizes[@]}"; do
    for interval in "${time_intervals[@]}"; do
        set -- $interval
        beginHour=$1
        endHour=$2
        serviceBeginTime=$3
        serviceEndTime=$4

        outputVehiclesPath="data/supply/transit_opt_paper/drt_fleet_templates/$outputSubdirectory/drt_fleet_${fleet_size}_${beginHour}-${endHour}.xml"

        echo "Creating: $outputVehiclesPath"

        java -cp $CLASSPATH com.husseinmahfouz.matsim.drt.RunCreateDrtVehicles \
            --network-path $networkPath \
            --output-vehicles-path $outputVehiclesPath \
            --vehicles-number $fleet_size \
            --vehicles-capacity $vehiclesCapacity \
            --service-begin-time $serviceBeginTime \
            --service-end-time $serviceEndTime \
            --random-seed $randomSeed \
            --vehicle-id-prefix $vehicleIdPrefix \
            --service-area-path $serviceAreaPath \
            --network-modes $networkModes
    done
done

echo "All DRT vehicle files generated successfully."