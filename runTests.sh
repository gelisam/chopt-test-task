#!/bin/bash
set -e

stack install

clear

chopt-test-task slave localhost 8080 &
chopt-test-task slave localhost 8081 &
chopt-test-task slave localhost 8082 &
chopt-test-task slave localhost 8083 &
sleep 1
chopt-test-task master localhost 8084
