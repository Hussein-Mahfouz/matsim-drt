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
fleet_sizes=(0 10 25 50 100 150)

# Define time intervals (beginHour endHour serviceBeginTime serviceEndTime)
# We add a +/- 300s (5 min) overlap to prevent crashes at the handover points.
time_intervals=(
    "0 4 0 14700"          # Ends 5 mins late (04:05)
    "4 8 14100 29100"      # Starts 5 mins early (03:55), Ends 5 mins late (08:05)
    "8 12 28500 43500"     # Starts 5 mins early (07:55), Ends 5 mins late (12:05)
    "12 16 42900 57900"    # Starts 5 mins early (11:55), Ends 5 mins late (16:05) - FIXES YOUR CRASH
    "16 20 57300 72300"    # Starts 5 mins early (15:55), Ends 5 mins late (20:05)
    "20 24 71700 86400"    # Starts 5 mins early (19:55), Ends at midnight
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