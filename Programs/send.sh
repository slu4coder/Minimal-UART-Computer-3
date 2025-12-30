#!/bin/bash

# Send input with line delay
while IFS= read -r line; do
  echo -ne "${line}\n" > /dev/ttyUSB0 # /dev/stdout for testing
  sleep 0.01 # wait for the Minimal Computer to decode a line
done

