#!/bin/sh 
echo "Running using ${1} processes"
mpirun -np ${1} ./dist/build/q2/q2
