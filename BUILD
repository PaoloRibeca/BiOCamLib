#!/bin/sh

set -e

PROFILE="$1"
if [ -z "$PROFILE" ]; then
  PROFILE="dev"
fi

if [ "$PROFILE" = "release" ]; then
  rm -rf _build
fi

dune build --profile="$PROFILE" bin/Parallel.exe
dune build --profile="$PROFILE" bin/FASTools.exe

chmod 755 _build/default/bin/Parallel.exe _build/default/bin/FASTools.exe
if [ "$PROFILE" = "release" ]; then
  strip _build/default/bin/Parallel.exe _build/default/bin/FASTools.exe
fi

rm -f Parallel FASTools
ln -s _build/default/bin/Parallel.exe Parallel
ln -s _build/default/bin/FASTools.exe FASTools

