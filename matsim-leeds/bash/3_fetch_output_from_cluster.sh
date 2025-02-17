#!/bin/bash

# RUN FROM LOCAL MACHINE

# get variables from .env files
SCRIPT_DIR="$(dirname "$0")"
source "$SCRIPT_DIR/.env"


# Set the current working directory (assumes you are running the script from matsim-leeds/)
MATSIM_DIR="$(pwd)"

# Define remote and local directories relative to matsim-leeds
REMOTE_USER="hmahfouz"
REMOTE_HOST="euler.ethz.ch"


# Results directories (split for remote and local)
REMOTE_RESULTS_DIR="$REMOTE_DIR/scenarios/test/results_dmc"  # Remote directory for results
LOCAL_RESULTS_DIR="$MATSIM_DIR/scenarios/test/results_dmc"    # Local directory where results will be synced to



# Print the directories for confirmation
echo "Syncing from $REMOTE_RESULTS_DIR to $LOCAL_RESULTS_DIR"

# Use rsync to sync the results 
rsync -avz "$REMOTE_USER@$REMOTE_HOST:$REMOTE_RESULTS_DIR/" "$LOCAL_RESULTS_DIR/"
# to overwrite regardless of timestamp compariason
# rsync -avz --ignore-existing "$REMOTE_USER@$REMOTE_HOST:$REMOTE_RESULTS_DIR/" "$LOCAL_RESULTS_DIR/"

# Print message on successful completion
echo "Results have been synced successfully to $LOCAL_RESULTS_DIR"

