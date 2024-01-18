#!/usr/bin/env bash

set -e

PROFILE="$1"
if [[ "$PROFILE" == "" ]]; then
  PROFILE="dev"
fi

# Always erase build directory to ensure peace of mind
rm -rf _build

# Emit version info
echo -e "include (\n  struct\n    let info = {\n      Tools.Argv.name = \"BiOCamLib\";\n      version = \"$(git log --pretty=format: --name-only | awk '{if ($0!="") print}' | wc -l)\";\n      date = \"$(date -d "@$(git log -1 --format="%at")" +%d-%b-%Y)\"\n    }\n  end\n)" > lib/Info.ml

#FLAGS="--verbose"

dune build --profile="$PROFILE" bin/Parallel.exe $FLAGS
dune build --profile="$PROFILE" bin/Octopus.exe $FLAGS
dune build --profile="$PROFILE" bin/RC.exe $FLAGS
dune build --profile="$PROFILE" bin/FASTools.exe $FLAGS
dune build --profile="$PROFILE" bin/TREx.exe $FLAGS

rm -rf build
mkdir build

cp _build/default/bin/Parallel.exe build/Parallel
cp _build/default/bin/Octopus.exe build/Octopus
cp _build/default/bin/RC.exe build/RC
cp _build/default/bin/FASTools.exe build/FASTools
cp _build/default/bin/TREx.exe build/TREx

chmod 755 build/*

if [[ "$PROFILE" == "release" ]]; then
  strip build/*
  rm -rf _build
fi

