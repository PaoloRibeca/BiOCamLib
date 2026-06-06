#!/usr/bin/env bash

set -e

# ──────────────────────────────────────────────────────────────────────
# Special target: regenerate README.pdf from README.md
#   ./BUILD README.pdf
# Markdown -> self-contained HTML (pandoc, GitHub-flavoured, image + CSS
# embedded) -> PDF (headless Chrome/Chromium, the same engine GitHub's
# "print to PDF" uses).  Needs: pandoc and google-chrome/chromium.
# ──────────────────────────────────────────────────────────────────────
if [[ "${1:-}" == "README.pdf" ]]; then
  cd "$(dirname "${BASH_SOURCE[0]}")"
  command -v pandoc >/dev/null 2>&1 || { echo "BUILD: pandoc not found" >&2; exit 1; }
  CHROME=""
  for c in google-chrome google-chrome-stable chromium chromium-browser; do
    if command -v "$c" >/dev/null 2>&1; then CHROME="$c"; break; fi
  done
  [[ -n "$CHROME" ]] || { echo "BUILD: no google-chrome/chromium found" >&2; exit 1; }
  HTML="$(mktemp --suffix=.html)"
  # Private profile dir so this Chrome never blocks on the default-profile
  # singleton lock held by an unrelated Chrome already running on the host.
  UDD="$(mktemp -d)"
  # Render to a temp PDF; only move it into place once it is a valid PDF,
  # so a Chrome crash never leaves a stale README.pdf looking like success.
  PDF="$(mktemp --suffix=.pdf)"
  LOG="$(mktemp)"
  trap 'rm -rf "$HTML" "$UDD" "$PDF" "$LOG"' EXIT
  # --embed-resources (pandoc >= 2.19) supersedes the older --self-contained;
  # use whichever this pandoc advertises so the build also works on older installs.
  EMBED=--embed-resources
  pandoc --help 2>/dev/null | grep -q -- --embed-resources || EMBED=--self-contained
  pandoc README.md -f gfm -t html5 --standalone "$EMBED" \
         --css README.css --metadata title="BiOCamLib" -o "$HTML"
  # --disable-background-networking is essential: the GCM/SSL phone-home it
  # otherwise attempts stalls — and on this Chrome crashes (dangling raw_ptr)
  # — the headless render of a large image-heavy page.  The rest are headless
  # hygiene so the render is fully self-contained.
  if "$CHROME" --headless=new --no-sandbox --disable-gpu \
       --disable-dev-shm-usage --disable-background-networking \
       --disable-default-apps --disable-extensions --disable-sync \
       --disable-component-update --no-first-run --metrics-recording-only \
       --user-data-dir="$UDD" --no-pdf-header-footer \
       --print-to-pdf="$PDF" "$HTML" 2>"$LOG" \
     && [[ -s "$PDF" ]] && [[ "$(head -c4 "$PDF")" == "%PDF" ]]; then
    mv "$PDF" README.pdf
    echo "BUILD: wrote README.pdf"
  else
    echo "BUILD: README.pdf FAILED — pandoc or headless Chrome error:" >&2
    sed 's/^/    /' "$LOG" >&2
    exit 1
  fi
  exit 0
fi

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
dune build --profile="$PROFILE" bin/AnnoTools.exe $FLAGS
dune build --profile="$PROFILE" bin/TREx.exe $FLAGS
dune build --profile="$PROFILE" bin/Cophenetic.exe $FLAGS
dune build --profile="$PROFILE" bin/Yggdrasill.exe $FLAGS

rm -rf build
mkdir build

cp _build/default/bin/Parallel.exe build/Parallel
cp _build/default/bin/Octopus.exe build/Octopus
cp _build/default/bin/RC.exe build/RC
cp _build/default/bin/FASTools.exe build/FASTools
cp _build/default/bin/AnnoTools.exe build/AnnoTools
cp _build/default/bin/TREx.exe build/TREx
cp _build/default/bin/Cophenetic.exe build/Cophenetic
cp _build/default/bin/Yggdrasill.exe build/Yggdrasill

chmod 755 build/*

if [[ "$PROFILE" == "release" || "$PROFILE" == "release-static" ]]; then
  strip build/*
  rm -rf _build
fi

