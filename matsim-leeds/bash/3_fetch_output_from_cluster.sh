#!/bin/bash

# RUN FROM LOCAL MACHINE

# get variables from .env files
SCRIPT_DIR="$(dirname "$0")"
source "$SCRIPT_DIR/.env"

# Set the current working directory (assumes you are running the script from matsim-leeds/)
MATSIM_DIR="$(pwd)"

# List of directory names to exclude. All directories with these names will be excluded from the 
# rsync command. This is useful for excluding large directories (Such as ITERS) that are not needed locally. 
 
EXCLUDE_DIRS=("ITERS" "sample_0.05")
EXCLUDE_FILES=("output_events.xml.gz" "output_plans.xml.gz")



# Declare an associative array to map remote directories to their corresponding local directories
declare -A DIR_PAIRS=(
    # ["$REMOTE_DIR/scenarios/fleet_sizing_minCostFlow/all"]="$MATSIM_DIR/scenarios/fleet_sizing/all"
    # ["$REMOTE_DIR/scenarios/fleet_sizing_minCostFlow/zones"]="$MATSIM_DIR/scenarios/fleet_sizing/zones"
    # ["$REMOTE_DIR/scenarios/fleet_sizing_minCostFlow/inner"]="$MATSIM_DIR/scenarios/fleet_sizing/inner"
    # ["$REMOTE_DIR/scenarios/fleet_sizing_minCostFlow/innerBUA"]="$MATSIM_DIR/scenarios/fleet_sizing/innerBUA"
    # ["$REMOTE_DIR/scenarios/basic/sample_1.00"]="$MATSIM_DIR/scenarios/basic/sample_1.00"
)

# Build the rsync exclude options
EXCLUDE_OPTIONS=()
for DIR in "${EXCLUDE_DIRS[@]}"; do
    EXCLUDE_OPTIONS+=("--exclude=*/${DIR}/")
done

for FILE in "${EXCLUDE_FILES[@]}"; do
    EXCLUDE_OPTIONS+=("--exclude=${FILE}")
done

# Loop through each pair of directories and run rsync
for REMOTE_DIR in "${!DIR_PAIRS[@]}"; do
    # Retrieve the corresponding local directory for the current remote directory
    LOCAL_DIR="${DIR_PAIRS[$REMOTE_DIR]}"

    # Print the directories for confirmation
    echo "Syncing from $REMOTE_DIR to $LOCAL_DIR"
    # Create the local directory if it does not exist
    mkdir -p "$LOCAL_DIR"

    # Use rsync to sync the results, excluding specified directories
    rsync -avz -vv "${EXCLUDE_OPTIONS[@]}" "$REMOTE_USER@$REMOTE_HOST:$REMOTE_DIR/" "$LOCAL_DIR/"

    # Print message on successful completion
    echo "Results have been synced successfully to $LOCAL_DIR"
done

