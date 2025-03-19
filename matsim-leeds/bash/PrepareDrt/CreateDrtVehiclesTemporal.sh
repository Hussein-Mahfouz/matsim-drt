#!/bin/bash

# Run this file from the following directory: matsim-leeds
# If file is in matsim-leeds/bash, run ./bash/CreateDrtVehiclesByTime.sh

# Define the fixed arguments
networkPath="data/supply/network_mapped.xml.gz"
randomSeed="1234"
networkModes="car"

# Define the service area arguments
serviceAreaArgs="drt_fleet_nw_ data/supply/drt/nw_cluster_08_00_11_00.shp 4 nw"
# serviceAreaArgs="drt_fleet_ne_ data/supply/drt/ne_cluster_08_00_11_00.shp 4"


# Define the list of time ranges (in hours, seconds, and fleet size)
declare -a timeRanges=(
    "5 8 18000 28800 50"  # 5:00 - 8:00, 50 vehicles
    "8 11 28800 39600 100"  # 8:00 - 11:00, 100 vehicles
    "11 13 39600 46800 150"  # 11:00 - 13:00, 150 vehicles
    "14 17 50400 61200 200"  # 14:00 - 17:00, 200 vehicles
    "17 20 61200 72000 250"  # 17:00 - 20:00, 250 vehicles
)

# Get the classpath for all dependencies and append target/classes (the former has core functionality e.g. core eqasim, the latter has the classes I wrote and compile)
mvn dependency:build-classpath -Dmdep.outputFile=cp.txt
CLASSPATH=$(cat cp.txt):target/classes

# Extract service area arguments
set -- $serviceAreaArgs
vehicleIdPrefix=$1
serviceAreaPath=$2
vehiclesCapacity=$3
subdirectory=$4

# Check if the shapefile exists
if [ ! -f "$serviceAreaPath" ]; then
    echo "Shapefile not found: $serviceAreaPath"
    exit 1
fi

# Create the subdirectory if it doesn't exist
mkdir -p "data/supply/drt/$subdirectory/fleets/temporal/"

# Loop through the time ranges to run the Java program
for timeRange in "${timeRanges[@]}"; do
    set -- $timeRange
    beginHour=$1
    endHour=$2
    serviceBeginTime=$3
    serviceEndTime=$4
    fleetSize=$5

    outputVehiclesPath="data/supply/drt/$subdirectory/fleets/temporal/${vehicleIdPrefix}_${beginHour}-${endHour}_${fleetSize}.xml"

    # Log the output file path
    echo "Creating vehicles file: $outputVehiclesPath"

    java -cp $CLASSPATH com.husseinmahfouz.matsim.drt.RunCreateDrtVehicles \
        --network-path $networkPath \
        --output-vehicles-path $outputVehiclesPath \
        --vehicles-number $fleetSize \
        --vehicles-capacity $vehiclesCapacity \
        --service-begin-time $serviceBeginTime \
        --service-end-time $serviceEndTime \
        --random-seed $randomSeed \
        --vehicle-id-prefix $vehicleIdPrefix \
        --service-area-path $serviceAreaPath \
        --network-modes $networkModes
done