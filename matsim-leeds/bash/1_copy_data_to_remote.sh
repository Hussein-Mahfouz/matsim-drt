#!/bin/bash

# RUN FROM LOCAL MACHINE

# If using LOCAL_DIR=$(pwd), always run this script from matsim-leeds

# get variables from .env files
SCRIPT_DIR="$(dirname "$0")"
source "$SCRIPT_DIR/.env"


# Get the current local directory
LOCAL_DIR="$(pwd)"

echo "Current Local Directory: $LOCAL_DIR"

# List of files and directories to copy (can include both files and directories)
FILES_AND_DIRS=("$LOCAL_DIR/data" "$LOCAL_DIR/scenarios" "$LOCAL_DIR/target/matsim-leeds-1.0.jar")

# Loop through each item (file or subdirectory) and run rsync for each
for ITEM in "${FILES_AND_DIRS[@]}"; do
    echo "Syncing $ITEM to $REMOTE_USER@$REMOTE_HOST:$REMOTE_DIR"
    rsync -avz "$ITEM" "$REMOTE_USER@$REMOTE_HOST:$REMOTE_DIR"
done

echo "Sync completed successfully!"



