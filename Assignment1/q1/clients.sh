#!/bin/sh

for ((i=1; i<=$1; i++)); do
   echo "Starting client $i"
   ./dist/build/NPlayerClient/NPlayerClient &
done
