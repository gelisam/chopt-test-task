#!/bin/bash
set -e

stack install

clear

cat nodelist.txt | while IFS='' read -r NODE_CONFIG; do
  # NODE_CONFIG looks like "slave localhost 8080",
  # we want it to expand to multiple arguments
  echo chopt-test-task $NODE_CONFIG &
  chopt-test-task $NODE_CONFIG &
done
