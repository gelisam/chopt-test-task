#!/bin/bash
set -e

stack install

# If the arguments given to this script are incorrect, they will be sent to all
# the nodes and the error message will be unreadable. To avoid duplicating the
# argument-parsing logic here, we use a special role which tests the arguments.
chopt-test-task check-args "$@"

./scripts/run_tests_and_hang.sh "$@" &
PID="$!"
trap "./scripts/kill_recursively.sh $PID" EXIT


# wait for the child process to terminate
wait
