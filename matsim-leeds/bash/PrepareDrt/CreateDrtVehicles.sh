#!/bin/bash

# Run this file from the following directory: matsim-leeds
# If file is in matsim-leeds/bash, run ./bash/CreateDrtFleets.sh

# This script takes a set of DRT service areas (each with a different service area), and creates a set of DRT fleets for each service area.
# The fleets are created with different sizes (50, 100, 200, 500, 1000) to test out different DRT fleet size performance.

# One optional argument is the service area shapefile. If you want a DRT fleet for the entire study area, just pass "all_study_area" as an argument 
# instead of a path to a shapefile.

# Define the fixed arguments
networkPath="data/supply/network_mapped.xml.gz"
randomSeed="1234"
networkModes="car"

# Define the different sets of arguments for service areas (service area path can be "all_study_area" to indicate no specific service area)
declare -a serviceAreaArgsArray=(
    "drt_fleet_nw_ data/supply/drt/nw_cluster_08_00_11_00.shp 4 nw"
    "drt_fleet_ne_ data/supply/drt/ne_cluster_08_00_11_00.shp 4 ne"
    "drt_fleet_all_ all_study_area 4 all"  # No specific service area
)

# Define the list of fleet sizes
declare -a fleetSizes=(50 100 200 500 1000)

# Get the classpath for all dependencies and append target/classes (the former has core functionality e.g., core eqasim, the latter has the classes I wrote and compile)
mvn dependency:build-classpath -Dmdep.outputFile=cp.txt
CLASSPATH=$(cat cp.txt):target/classes

# Loop through the service area arguments and fleet sizes to run the Java program
for serviceAreaArgs in "${serviceAreaArgsArray[@]}"; do
    set -- $serviceAreaArgs
    vehicleIdPrefix=$1
    serviceAreaPath=$2
    vehiclesCapacity=$3
    subdirectory=$4

    # Create the subdirectory if it doesn't exist
    mkdir -p "data/supply/drt/$subdirectory/fleets/"

    for fleetSize in "${fleetSizes[@]}"; do
        outputVehiclesPath="data/supply/drt/$subdirectory/fleets/${vehicleIdPrefix}${fleetSize}.xml"

        # Log the output file path
        echo "Creating vehicles file: $outputVehiclesPath"

        if [ "$serviceAreaPath" != "all_study_area" ]; then
            # Check if the shapefile exists
            if [ ! -f "$serviceAreaPath" ]; then
                echo "Shapefile not found: $serviceAreaPath"
                continue
            fi

            java -cp $CLASSPATH com.husseinmahfouz.matsim.drt.RunCreateDrtVehicles \
                --network-path $networkPath \
                --output-vehicles-path $outputVehiclesPath \
                --vehicles-number $fleetSize \
                --vehicles-capacity $vehiclesCapacity \
                --service-begin-time 0 \
                --service-end-time 86400 \
                --random-seed $randomSeed \
                --vehicle-id-prefix $vehicleIdPrefix \
                --service-area-path $serviceAreaPath \
                --network-modes $networkModes
        else
            java -cp $CLASSPATH com.husseinmahfouz.matsim.drt.RunCreateDrtVehicles \
                --network-path $networkPath \
                --output-vehicles-path $outputVehiclesPath \
                --vehicles-number $fleetSize \
                --vehicles-capacity $vehiclesCapacity \
                --service-begin-time 0 \
                --service-end-time 86400 \
                --random-seed $randomSeed \
                --vehicle-id-prefix $vehicleIdPrefix \
                --network-modes $networkModes
        fi
    done
done