#!/bin/sh

PROFILE="$1"
if [ -z "$PROFILE" ]; then
  PROFILE="dev"
fi

dune --profile="$PROFILE" build bin/Parallel.exe

chmod 755 _build/default/bin/Parallel.exe

rm -f Parallel
ln -s _build/default/bin/Parallel.exe Parallel

