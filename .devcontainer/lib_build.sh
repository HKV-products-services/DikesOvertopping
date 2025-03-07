#!/bin/bash
# This script handles the build process for the DikesOvertopping project

# Check if workspace folder argument is provided
if [ -z "$1" ]; then
    echo "Error: Workspace folder path must be provided as the first argument"
    echo "Usage: $0 <workspace_folder_path>"
    exit 1
fi

# Get the workspace folder from the first argument
WORKSPACE_FOLDER="$1"
echo "Using workspace folder: $WORKSPACE_FOLDER"

# Step 1: Clean and prepare build directory
echo "Cleaning and preparing build directory..."
rm -rf $WORKSPACE_FOLDER/build/*
mkdir -p $WORKSPACE_FOLDER/build
cd $WORKSPACE_FOLDER/build

# Step 2: Configure and build
echo "Configuring with CMake..."
cmake ../src/core

echo "Building the project..."
make

# Step 3: Create symbolic link for VS Code
echo "Creating symbolic link for VS Code..."
ln -sf $WORKSPACE_FOLDER/build /code/build

# Step 4: Clean up build directory, keeping only specific files
echo "Cleaning up build directory, keeping only essential files..."
# First, make sure the directories exist
mkdir -p $WORKSPACE_FOLDER/build/external

# Create a temporary directory to store the files we want to keep
mkdir -p $WORKSPACE_FOLDER/build/temp
# Move the files we want to keep to the temporary directory
if [ -f "$WORKSPACE_FOLDER/build/libDikesOvertopping.so" ]; then
    cp $WORKSPACE_FOLDER/build/libDikesOvertopping.so $WORKSPACE_FOLDER/build/temp/
fi
if [ -f "$WORKSPACE_FOLDER/build/external/libFeedbackDll.so" ]; then
    cp $WORKSPACE_FOLDER/build/external/libFeedbackDll.so $WORKSPACE_FOLDER/build/temp/
fi

# Remove everything except the temp directory
find $WORKSPACE_FOLDER/build -mindepth 1 -not -path "$WORKSPACE_FOLDER/build/temp*" -delete

# Move the files back
if [ -f "$WORKSPACE_FOLDER/build/temp/libDikesOvertopping.so" ]; then
    mv $WORKSPACE_FOLDER/build/temp/libDikesOvertopping.so $WORKSPACE_FOLDER/build/
fi
if [ -f "$WORKSPACE_FOLDER/build/temp/libFeedbackDll.so" ]; then
    mv $WORKSPACE_FOLDER/build/temp/libFeedbackDll.so $WORKSPACE_FOLDER/build/
fi

# Remove the temporary directory
rm -rf $WORKSPACE_FOLDER/build/temp

echo "Build completed successfully! Only essential library files are kept."
