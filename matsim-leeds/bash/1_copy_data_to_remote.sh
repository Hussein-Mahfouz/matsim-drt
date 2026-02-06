#!/bin/bash

# RUN FROM LOCAL MACHINE
# This script syncs specific files and directories from the local machine to a remote server.
# Ensure that `.env` contains the correct REMOTE_USER, REMOTE_HOST, and REMOTE_DIR variables.

# Get the script directory and load variables from the .env file
SCRIPT_DIR="$(dirname "$0")"
source "$SCRIPT_DIR/.env"

# Get the current local directory where the script is run
LOCAL_DIR="$(pwd)"
echo "Current Local Directory: $LOCAL_DIR"

# Declare an associative array to map local paths to their corresponding remote paths
declare -A FILES_AND_DIRS=(
    ["data/supply/transit_opt_paper/drt_fleet_templates"]="data/supply/transit_opt_paper/drt_fleet_templates"
    #["src/main/resources"]="src/main/resources"
    #["target"]="target"
)

# Loop through each item in the array and run rsync
for ITEM in "${!FILES_AND_DIRS[@]}"; do
    LOCAL_PATH="$LOCAL_DIR/$ITEM"
    REMOTE_PATH="$REMOTE_USER@$REMOTE_HOST:$REMOTE_DIR/${FILES_AND_DIRS[$ITEM]}"

    # Check if the item is a directory
    if [[ -d "$LOCAL_PATH" ]]; then
        echo "Syncing directory $ITEM to $REMOTE_PATH"
        # Use a trailing slash to sync only the contents of the directory
        rsync -avz --progress \
            --exclude='schedule_unmapped.xml' \
            "$LOCAL_PATH/" "$REMOTE_PATH"
    elif [[ -f "$LOCAL_PATH" ]]; then
        echo "Syncing file $ITEM to $REMOTE_PATH"
        # Sync the file directly without the trailing slash
        rsync -avz --progress "$LOCAL_PATH" "$REMOTE_PATH"
    else
        echo "Error: $LOCAL_PATH does not exist or is not a valid file/directory. Skipping..."
    fi
done

echo "Sync completed successfully!"
