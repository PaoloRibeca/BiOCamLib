#!/usr/bin/env bash

set -e

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# Everything below is relative to the tree this script sits in, and not to
# wherever it was invoked from.
cd "$ROOT"

# Everything below that is not specific to this repository lives in tools/,
# which every repository of the family reaches through its BiOCamLib submodule
# -- this one reaches its own copy.  What stays here is the list of what this
# repository builds and the profiles it builds with.
TOOLS="$ROOT/tools"

# Every dune invocation names the root explicitly, as the tools above do.
# Without it dune takes the OUTERMOST enclosing dune-project, which for a tree
# checked out inside another one -- a git worktree under .claude/, say -- is a
# different project altogether, and the build then either fails or silently
# builds the wrong tree.
DUNE=(dune build --root "$ROOT")

if [[ "${1:-}" == "README.pdf" ]]; then
  bash "$TOOLS/markdown-pdf" --root "$ROOT" --title BiOCamLib
  exit 0
fi

# The assertion suite (test/Tests.exe, whose harness is BiOCamLib.Testing).
#   ./BUILD test [<profile>]   build and run it without rebuilding the binaries
# It is also run at the end of every ordinary build.  A non-zero exit means
# either a check failed or a known-bug marker went stale, and both should stop
# a build.
run_tests() {
  local profile="${1:-$PROFILE}"
  echo
  "${DUNE[@]}" --profile="$profile" test/Tests.exe $FLAGS
  ./_build/default/test/Tests.exe
}

# Release packaging and the macOS CI live in tools/release, which takes the
# project name and reads the rest from releases/MANIFEST:
#   ./BUILD package [<ver>]   assemble releases/BiOCamLib-<ver>-<os>-<arch>.tar.xz
#   ./BUILD mac-begin         tag v<CURRENT> and push it, triggering the CI
#   ./BUILD mac-end           wait for it, download the macOS binaries, package
if [[ "${1:-}" == "package" ]]; then
  bash "$TOOLS/release" package "${2:-}" --root "$ROOT" --name BiOCamLib
  exit 0
fi

if [[ "${1:-}" == "mac-begin" ]]; then
  bash "$TOOLS/release" mac-begin --root "$ROOT"
  exit 0
fi

if [[ "${1:-}" == "mac-end" ]]; then
  bash "$TOOLS/release" mac-end --root "$ROOT" --name BiOCamLib
  exit 0
fi

if [[ "${1:-}" == "test" ]]; then
  PROFILE="${2:-dev}"
  run_tests "$PROFILE"
  exit 0
fi

PROFILE="$1"
if [[ "$PROFILE" == "" ]]; then
  PROFILE="dev"
fi

# Always erase build directory to ensure peace of mind
rm -rf _build

# Emit version info.  The logic lives in stamp-version, which every repository
# of the family reaches through its BiOCamLib submodule, so that none of them
# carries a second copy of it to drift.  The nine binaries below take their
# version from the same module through Info.for_program: they ship in one
# archive from one tree, so they share its version and differ only in name.
# The library first, then every binary this repository produces -- so the list
# of what it produces lives here rather than in a literal inside each of them.
bash "$TOOLS/stamp-version" --root "$ROOT" --out "$ROOT/lib/Info.ml" \
  BiOCamLib AnnoTools Cophenetic FASTools NJ Octopus Parallel RC TREx Yggdrasill

#FLAGS="--verbose"

"${DUNE[@]}" --profile="$PROFILE" bin/Parallel.exe $FLAGS
"${DUNE[@]}" --profile="$PROFILE" bin/Octopus.exe $FLAGS
"${DUNE[@]}" --profile="$PROFILE" bin/RC.exe $FLAGS
"${DUNE[@]}" --profile="$PROFILE" bin/FASTools.exe $FLAGS
"${DUNE[@]}" --profile="$PROFILE" bin/AnnoTools.exe $FLAGS
"${DUNE[@]}" --profile="$PROFILE" bin/TREx.exe $FLAGS
"${DUNE[@]}" --profile="$PROFILE" bin/Cophenetic.exe $FLAGS
"${DUNE[@]}" --profile="$PROFILE" bin/NJ.exe $FLAGS
"${DUNE[@]}" --profile="$PROFILE" bin/Yggdrasill.exe $FLAGS

rm -rf .build
mkdir .build

cp _build/default/bin/Parallel.exe .build/Parallel
cp _build/default/bin/Octopus.exe .build/Octopus
cp _build/default/bin/RC.exe .build/RC
cp _build/default/bin/FASTools.exe .build/FASTools
cp _build/default/bin/AnnoTools.exe .build/AnnoTools
cp _build/default/bin/TREx.exe .build/TREx
cp _build/default/bin/Cophenetic.exe .build/Cophenetic
cp _build/default/bin/NJ.exe .build/NJ
cp _build/default/bin/Yggdrasill.exe .build/Yggdrasill

chmod 755 .build/*

# Build and run the assertion suite.  Tests exits non-zero when a check fails
# OR when a known-bug marker has gone stale -- i.e. a check pinning a diagnosed
# defect has started passing, so the marker must be removed.  Both are build
# failures: 'set -e' stops us here, before the binaries are stripped.
run_tests

if [[ "$PROFILE" == "release" || "$PROFILE" == "release-static" ]]; then
  strip .build/*
  rm -rf _build
fi

