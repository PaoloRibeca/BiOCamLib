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

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# ──────────────────────────────────────────────────────────────────────
# Release packaging targets (BiOCamLib is pure OCaml, so the macOS binaries
# are built exactly like the Linux ones, no C/C++ path):
#   ./BUILD package [<ver>]   assemble releases/BiOCamLib-<ver>-<os>-<arch>.tar.xz
#                             from releases/MANIFEST.  <ver> is N.N.N; omitted
#                             it is read from releases/CURRENT, given it replaces
#                             CURRENT (old one kept as CURRENT~).  Builds the
#                             binaries first if none are present (release-static
#                             on Linux, release on macOS).  Set PACKAGE_PLATFORM
#                             + PACKAGE_BINDIR to package downloaded binaries for
#                             another platform (see mac-end).
#   ./BUILD mac-begin         tag v<CURRENT> on HEAD (if absent) + push it,
#                             triggering the macOS CI (build-binaries.yml).
#   ./BUILD mac-end           wait for that CI run, download the macOS binaries
#                             and 'package' them into releases/ (needs gh).
# ──────────────────────────────────────────────────────────────────────

# Assemble a distributable package (tarball) under releases/.  The package dir
# is BiOCamLib-<version>-<os>-<arch> (os/arch from uname, so the same target
# works on Linux and macOS), populated from releases/MANIFEST — a two-column
# list mapping each source path in the tree (first column) to its name inside
# the package (second column) — then tar'd and xz-compressed.  A '.build/<name>'
# entry is a compiled binary (taken from the local build tree, or from
# PACKAGE_BINDIR when packaging downloaded binaries for another platform); every
# other entry is copied from the source tree.
build_package() {
  local version_arg="${1:-}"
  local manifest="$ROOT/releases/MANIFEST"
  local current="$ROOT/releases/CURRENT"
  local version_re='^[0-9]+\.[0-9]+\.[0-9]+$'
  local version

  command -v xz >/dev/null 2>&1 \
    || { echo "(BUILD package): xz not found" >&2; exit 1; }
  [[ -f "$manifest" ]] \
    || { echo "(BUILD package): releases/MANIFEST not found" >&2; exit 1; }

  if [[ -n "$version_arg" ]]; then
    # Version given on the command line: validate it, then make it the new
    # CURRENT, preserving the previous one as CURRENT~.
    [[ "$version_arg" =~ $version_re ]] \
      || { echo "(BUILD package): '$version_arg' is not a valid version (expected N.N.N)" >&2; exit 1; }
    version="$version_arg"
    if [[ -f "$current" ]]; then mv -f "$current" "$current~"; fi
    printf '%s\n' "$version" > "$current"
  else
    # No version given: take it from CURRENT, which must exist and be valid.
    [[ -f "$current" ]] \
      || { echo "(BUILD package): no version given and releases/CURRENT not found" >&2; exit 1; }
    read -r version < "$current" || true
    [[ "$version" =~ $version_re ]] \
      || { echo "(BUILD package): releases/CURRENT does not hold a valid version (expected N.N.N)" >&2; exit 1; }
  fi

  # Target platform tag: normally the host (uname -s '-' uname -m), but a
  # cross-built package is assembled from downloaded binaries by pointing
  # PACKAGE_PLATFORM at the <os>-<arch> tag and PACKAGE_BINDIR at their
  # directory.  Dash-separated fields (e.g. BiOCamLib-1.2.2-Linux-x86_64) let the
  # parts be recovered by splitting on '-': neither os nor arch carries a dash on
  # the platforms we target, and arch keeps its underscore (x86_64).
  local platform name dir
  if [[ -n "${PACKAGE_PLATFORM:-}" ]]; then
    [[ "$PACKAGE_PLATFORM" =~ ^[A-Za-z0-9_-]+$ ]] \
      || { echo "(BUILD package): PACKAGE_PLATFORM '$PACKAGE_PLATFORM' has unexpected characters (want e.g. Darwin-arm64)" >&2; exit 1; }
    platform="$PACKAGE_PLATFORM"
  else
    platform="$(uname -s)-$(uname -m)"
  fi
  name="BiOCamLib-${version}-${platform}"
  dir="$ROOT/releases/$name"

  # Where the compiled binaries (the .build/<name> MANIFEST entries) come from.
  local bindir
  if [[ -n "${PACKAGE_BINDIR:-}" ]]; then
    # Cross-built package: take the binaries from the downloaded directory and
    # compile nothing locally.
    bindir="$PACKAGE_BINDIR"
    [[ -d "$bindir" ]] \
      || { echo "(BUILD package): PACKAGE_BINDIR '$bindir' is not a directory" >&2; exit 1; }
  else
    # Native package: build the binaries first if none are present.  release-static
    # links statically so the Linux binaries run on hosts lacking the shared libs;
    # macOS has no static libSystem, so -ccopt -static won't link there — use plain
    # release.
    bindir="$ROOT/.build"
    local profile
    case "$(uname -s)" in
      Darwin) profile="release" ;;
      *)      profile="release-static" ;;
    esac
    if [[ ! -x "$bindir/Parallel" ]]; then
      echo "(BUILD package): .build/Parallel missing — building with '$profile' first ..."
      ( cd "$ROOT" && bash BUILD "$profile" )
    fi
  fi

  echo "(BUILD package): assembling $name ..."
  rm -rf "$dir"
  mkdir -p "$dir"

  # Populate from MANIFEST: first column = source path in the tree, second
  # column = name inside the package.  A missing source fails the build loudly
  # rather than yielding a half-empty archive.
  local src dst from
  while read -r src dst; do
    [[ -z "$src" ]] && continue          # blank line
    case "$src" in \#*) continue;; esac  # comment line
    [[ -n "$dst" ]] \
      || { echo "(BUILD package): malformed MANIFEST line for '$src' (need two columns)" >&2; exit 1; }
    case "$src" in
      .build/*) from="$bindir/${src#.build/}" ;;
      *)       from="$ROOT/$src" ;;
    esac
    [[ -f "$from" ]] \
      || { echo "(BUILD package): '$src' -> '$from' not found — build it first?" >&2; exit 1; }
    cp -p "$from" "$dir/$dst"
    # Restore the executable bit on binaries: GitHub artifacts are zipped and
    # lose Unix permissions, so a downloaded binary may arrive non-executable.
    case "$src" in .build/*) chmod +x "$dir/$dst" ;; esac
  done < "$manifest"

  # tar the directory, then compress the archive with xz.  Run from within
  # releases/ so the archive holds paths relative to it (no releases/ prefix).
  echo "(BUILD package): creating $name.tar.xz ..."
  ( cd "$ROOT/releases" && tar cf "$name.tar" "$name" && xz -f "$name.tar" )
  echo "(BUILD package): done — releases/$name.tar.xz"
}

# Kick off the macOS CI build.  Reads the version from releases/CURRENT, forms
# the tag v<version>, creates it on HEAD if it does not exist yet, and pushes it
# to origin — which triggers .github/workflows/build-binaries.yml.  Pair with
# 'bash BUILD mac-end' to collect and package the resulting binaries.
build_mac_begin() {
  local current="$ROOT/releases/CURRENT"
  local version_re='^[0-9]+\.[0-9]+\.[0-9]+$'
  local version tag
  [[ -f "$current" ]] \
    || { echo "(BUILD mac-begin): releases/CURRENT not found" >&2; exit 1; }
  read -r version < "$current" || true
  [[ "$version" =~ $version_re ]] \
    || { echo "(BUILD mac-begin): releases/CURRENT does not hold a valid version (expected N.N.N)" >&2; exit 1; }
  tag="v$version"
  if git -C "$ROOT" rev-parse -q --verify "refs/tags/$tag" >/dev/null; then
    echo "(BUILD mac-begin): tag $tag already exists — not recreating."
  else
    echo "(BUILD mac-begin): creating tag $tag on HEAD ..."
    git -C "$ROOT" tag "$tag"
  fi
  echo "(BUILD mac-begin): pushing $tag to origin (triggers the macOS CI) ..."
  git -C "$ROOT" push origin "$tag"
  echo "(BUILD mac-begin): done — run 'bash BUILD mac-end' to wait for the build,"
  echo "                   then download and package the macOS binaries."
}

# Collect the macOS CI build and package it.  Waits for the build-binaries run
# of tag v<version> (version from releases/CURRENT) to finish, downloads the
# Darwin-arm64 and Darwin-x86_64 binaries, and runs 'package' for each — so the
# macOS archives land in releases/ next to the Linux one.  Needs an authenticated
# gh (GitHub CLI): run 'gh auth login' first.
build_mac_end() {
  local current="$ROOT/releases/CURRENT"
  local version_re='^[0-9]+\.[0-9]+\.[0-9]+$'
  local version tag
  command -v gh >/dev/null 2>&1 \
    || { echo "(BUILD mac-end): gh (GitHub CLI) not found — needed to fetch the CI artifacts" >&2; exit 1; }
  gh auth status >/dev/null 2>&1 \
    || { echo "(BUILD mac-end): gh is not authenticated — run 'gh auth login' first" >&2; exit 1; }
  [[ -f "$current" ]] \
    || { echo "(BUILD mac-end): releases/CURRENT not found" >&2; exit 1; }
  read -r version < "$current" || true
  [[ "$version" =~ $version_re ]] \
    || { echo "(BUILD mac-end): releases/CURRENT does not hold a valid version (expected N.N.N)" >&2; exit 1; }
  tag="v$version"

  # Find the CI run for this tag.  It may take a few seconds to register after
  # the tag push, so poll briefly before giving up.
  echo "(BUILD mac-end): locating the macOS CI run for $tag ..."
  local run_id="" i
  for ((i = 0; i < 30; i++)); do
    run_id="$(gh run list --workflow build-binaries.yml --event push --branch "$tag" \
                --limit 1 --json databaseId --jq '.[0].databaseId // empty' 2>/dev/null || true)"
    [[ -n "$run_id" ]] && break
    sleep 5
  done
  [[ -n "$run_id" ]] \
    || { echo "(BUILD mac-end): no CI run found for $tag — did 'bash BUILD mac-begin' run?" >&2; exit 1; }

  # Wait for it to finish; --exit-status makes gh fail if the run concluded badly.
  echo "(BUILD mac-end): waiting for run $run_id to finish ..."
  gh run watch "$run_id" --exit-status

  # Download each macOS artifact into its own scratch directory and package it.
  # 'package' with no version argument reads releases/CURRENT, so it never
  # re-shuffles CURRENT and the archives share the Linux build's version.
  local staging
  staging="$(mktemp -d)"
  local arch platform
  for arch in arm64 x86_64; do
    platform="Darwin-$arch"
    echo "(BUILD mac-end): downloading and packaging $platform ..."
    mkdir -p "$staging/$platform"
    gh run download "$run_id" --name "BiOCamLib-$platform" --dir "$staging/$platform"
    PACKAGE_PLATFORM="$platform" PACKAGE_BINDIR="$staging/$platform" \
      bash "$ROOT/BUILD" package
  done
  rm -rf "$staging"
  echo "(BUILD mac-end): done — macOS packages are in releases/."
}

if [[ "${1:-}" == "package" ]]; then
  build_package "${2:-}"
  exit 0
fi

if [[ "${1:-}" == "mac-begin" ]]; then
  build_mac_begin
  exit 0
fi

if [[ "${1:-}" == "mac-end" ]]; then
  build_mac_end
  exit 0
fi

# ──────────────────────────────────────────────────────────────────────
# The assertion suite (test/RunTests.exe, driven by test/Testing.ml).
#   ./BUILD test [<profile>]   build and run it without rebuilding the binaries
# It is also run at the end of every ordinary build.  A non-zero exit means
# either a check failed or a known-bug marker went stale, and both should stop
# a build.
# ──────────────────────────────────────────────────────────────────────
run_tests() {
  local profile="${1:-$PROFILE}"
  echo
  dune build --profile="$profile" test/RunTests.exe $FLAGS
  ./_build/default/test/RunTests.exe
}

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

# Emit version info.  The date is formatted by git itself (--date=format) rather
# than `date -d @<ts>`, which is GNU-only and fails on macOS's BSD date; the
# version stays the git file-change count.
echo -e "include (\n  struct\n    let info = {\n      Tools.Argv.name = \"BiOCamLib\";\n      version = \"$(git log --pretty=format: --name-only | awk '{if ($0!="") print}' | wc -l)\";\n      date = \"$(git log -1 --format=%ad --date=format:'%d-%b-%Y')\"\n    }\n  end\n)" > lib/Info.ml

#FLAGS="--verbose"

dune build --profile="$PROFILE" bin/Parallel.exe $FLAGS
dune build --profile="$PROFILE" bin/Octopus.exe $FLAGS
dune build --profile="$PROFILE" bin/RC.exe $FLAGS
dune build --profile="$PROFILE" bin/FASTools.exe $FLAGS
dune build --profile="$PROFILE" bin/AnnoTools.exe $FLAGS
dune build --profile="$PROFILE" bin/TREx.exe $FLAGS
dune build --profile="$PROFILE" bin/Cophenetic.exe $FLAGS
dune build --profile="$PROFILE" bin/NJ.exe $FLAGS
dune build --profile="$PROFILE" bin/Yggdrasill.exe $FLAGS

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

# Build and run the assertion suite.  RunTests exits non-zero when a check fails
# OR when a known-bug marker has gone stale -- i.e. a check pinning a diagnosed
# defect has started passing, so the marker must be removed.  Both are build
# failures: 'set -e' stops us here, before the binaries are stripped.
run_tests

if [[ "$PROFILE" == "release" || "$PROFILE" == "release-static" ]]; then
  strip .build/*
  rm -rf _build
fi

