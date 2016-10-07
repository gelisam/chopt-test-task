#!/bin/bash
set -e

stack install

clear

# If the arguments given to this script are incorrect, they will be sent to all
# the nodes and the error message will be unreadable. To avoid duplicating the
# argument-parsing logic here, we use a special role which tests the arguments.
chopt-test-task check-args "$@"

cat nodelist.txt | while IFS='' read -r NODE_CONFIG; do
  # NODE_CONFIG looks like "--role=Slave --address=localhost:8080:0",
  # we want it to expand to multiple arguments
  chopt-test-task run-node "$@" $NODE_CONFIG &
done
