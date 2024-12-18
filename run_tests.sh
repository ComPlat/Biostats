#!/bin/bash

# Set the path to the test directory
TEST_DIR="./bs/inst/tinytest"

# Check if the directory exists
if [ ! -d "$TEST_DIR" ]; then
	echo "Error: Directory $TEST_DIR does not exist."
	exit 1
fi

# Run all R test files in the directory
echo "Running tests in $TEST_DIR..."

# Create a temporary log file
LOG_FILE=$(mktemp)

# Iterate over all R scripts in the test directory
for TEST_FILE in "$TEST_DIR"/*.R; do
	echo "Running: $TEST_FILE"

	# Execute the test file and append the output to the log
	Rscript "$TEST_FILE" >>"$LOG_FILE" 2>&1
done

# Check the log for FAILED tests
if grep -q "FAILED" "$LOG_FILE"; then
	echo "Some tests FAILED. See details below:"
	grep "FAILED" "$LOG_FILE"
else
	echo "All tests passed successfully!"
fi

# Remove the temporary log file
rm "$LOG_FILE"
