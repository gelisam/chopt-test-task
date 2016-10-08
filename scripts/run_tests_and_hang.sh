#!/bin/bash
set -e

# temporarily replacing spaces by semicolons so we iterate on lines, not words
for NODE_SEMICONFIG in $(cat nodelist.txt | tr ' ' ';'); do
  NODE_CONFIG="$(echo "$NODE_SEMICONFIG" | tr ';' ' ')"
  
  # NODE_CONFIG looks like "--role=Slave --address=localhost:8080:0",
  # we want it to expand to multiple arguments
  echo chopt-test-task run-node "$@" $NODE_CONFIG
  chopt-test-task run-node "$@" $NODE_CONFIG &
done


# wait for CTRL-C
while true; do
  sleep 100
done
