#!/usr/bin/env python3
"""pdfgaps -- report bottom-whitespace per page of a PDF, to find where a tall
figure got pushed onto a fresh page (leaving a big gap on the previous one).

    python3 pdfgaps.py README.pdf            # pages with >18% bottom whitespace
    python3 pdfgaps.py README.pdf --min 30

The LAST page's trailing whitespace is normal (the document just ends).  A
mid-document page over the threshold is a figure (or other monolithic block)
that did not fit and jumped to the next page.

Requires pdftocairo (poppler) on PATH, plus pillow.
"""
import argparse
import glob
import os
import subprocess
import sys
import tempfile
from PIL import Image


def bottom_whitespace(png):
    im = Image.open(png).convert("L")
    w, h = im.size
    px = im.load()
    last = 0
    for y in range(h):
        if any(px[x, y] < 230 for x in range(0, w, 3)):
            last = y
    return (h - last) / h * 100.0


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("pdf")
    ap.add_argument("--min", type=float, default=18.0,
                    help="report pages with more than MIN%% bottom whitespace (default 18)")
    a = ap.parse_args()
    with tempfile.TemporaryDirectory() as d:
        try:
            subprocess.run(["pdftocairo", "-png", "-r", "60", a.pdf, os.path.join(d, "p")],
                           check=True)
        except (OSError, subprocess.CalledProcessError) as e:
            sys.exit(f"pdftocairo failed: {e}")
        pages = sorted(glob.glob(os.path.join(d, "p-*.png")))
        hits = 0
        for f in pages:
            frac = bottom_whitespace(f)
            if frac > a.min:
                n = os.path.splitext(os.path.basename(f))[0].split("-")[-1]
                tail = "  (last page -- normal)" if f == pages[-1] else ""
                print(f"page {n}: {frac:.0f}% bottom whitespace{tail}")
                hits += 1
        if not hits:
            print("no pages over threshold")


if __name__ == "__main__":
    main()
