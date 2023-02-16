#!/bin/sh

set -e

PROFILE="$1"
if [ -z "$PROFILE" ]; then
  PROFILE="dev"
fi

# Always erase build directory to ensure peace of mind
rm -rf _build

#FLAGS="--verbose"

dune build --profile="$PROFILE" bin/Parallel.exe $FLAGS
dune build --profile="$PROFILE" bin/FASTools.exe $FLAGS
dune build --profile="$PROFILE" bin/Octopus.exe $FLAGS

rm -rf build
mkdir build

cp _build/default/bin/Parallel.exe build/Parallel
cp _build/default/bin/FASTools.exe build/FASTools
cp _build/default/bin/Octopus.exe build/Octopus

chmod 755 build/*

if [ "$PROFILE" = "release" ]; then
  strip build/*
fi

rm -rf _build

