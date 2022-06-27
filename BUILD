#!/bin/sh

PROFILE="$1"
if [ -z "$PROFILE" ]; then
  PROFILE="dev"
fi

dune build --profile="$PROFILE" bin/Parallel.exe
dune build --profile="$PROFILE" bin/FASTools.exe

chmod 755 _build/default/bin/Parallel.exe _build/default/bin/FASTools.exe

rm -f Parallel FASTools
ln -s _build/default/bin/Parallel.exe Parallel
ln -s _build/default/bin/FASTools.exe FASTools

