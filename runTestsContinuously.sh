#!/bin/bash
set -e

function kill_children {
  local PARENT_PID="$1"
  local CHILD_PID
  for CHILD_PID in `pgrep -P "$PARENT_PID"`; do
    kill_entire_family "$CHILD_PID"
  done
}

function kill_entire_family {
  local PARENT_PID="$1"
  kill_children "$PARENT_PID"
  kill "$PARENT_PID" || true
}


stack install fswatcher

./runTests.sh "$@" &
PID="$!"
trap "kill_entire_family $PID" EXIT

fswatcher --path src echo "restart" | while read X; do
  kill_entire_family "$PID"
  ./runTests.sh "$@" &
  PID="$!"
  trap "kill_entire_family $PID" EXIT
done
