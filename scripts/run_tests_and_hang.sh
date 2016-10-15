#!/bin/bash
set -e

# temporarily replacing spaces by semicolons so we iterate on lines, not words
for SEMI_ADDRESS in $(cat nodelist.txt | tr ' ' ';'); do
  ADDRESS="$(echo "$SEMI_ADDRESS" | tr ';' ' ')"
  
  echo chopt-test-task run-node "$@" --address="$ADDRESS"
  chopt-test-task run-node "$@" --address="$ADDRESS" &
done


# wait for all the child processes to terminate
for SEMI_ADDRESS in $(cat nodelist.txt | tr ' ' ';'); do
  wait
done
