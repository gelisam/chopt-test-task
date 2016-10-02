#!/bin/bash
set -e

stack install

clear

cat nodelist.txt | while IFS='' read -r NODE_CONFIG; do
  # NODE_CONFIG looks like "--role=Slave --host=localhost --port=8080",
  # we want it to expand to multiple arguments
  chopt-test-task --send-for=10 --wait-for=2 --with-seed=1234 $NODE_CONFIG &
done
