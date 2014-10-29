#!/bin/sh
SRC=plate-tectonics/src
g++ -c $SRC/sqrdmd.c
g++ -c $SRC/lithosphere.cpp
g++ -c $SRC/plate.cpp
g++ -c $SRC/platecapi.cpp -Isrc
rm *.a -f
ar rcs plate-tectonics.a *.o
rm *.o

