#!/usr/bin/env python3
"""figscale -- size a figure for a README and emit the paste-ready snippet.

Measures a raster figure's text size (the median height of character-sized
connected components ~= its x-height) and computes the display width that makes
that text ~= the README body text.  Then prints a Markdown snippet ready to drop
into README.md -- and, for a figure taller than ~half a page, the split-across-
pages dual element plus the `.print-splitfig` CSS rule to add to README.css.

    figscale.py docs/fig.png                       # measure + emit snippet
    figscale.py docs/fig.png --target 6 --alt "Two-pass calibration workflow."
    figscale.py fig.pdf                            # rasterise + trim a pdf first

Why: a glyph `g` px tall in a `W`-px-wide PNG renders at `g*d/W`, so
`display_width = target_px * PNG_width / median_glyph_px`.  Default target 6 =
body x-height for the house 12px README font.

Requires numpy, pillow, scipy; pdftocairo (poppler) for PDF input.
"""
import argparse
import os
import re
import subprocess
import sys
import tempfile
import numpy as np
from PIL import Image, ImageChops
from scipy import ndimage

PAGE_PX = 1016.0   # A4 content height @96dpi (297mm - 2*14mm margins)


def rasterise_pdf(pdf, dpi, out):
    """First page of a single-figure PDF -> trimmed PNG."""
    with tempfile.TemporaryDirectory() as d:
        base = os.path.join(d, "p")
        subprocess.run(["pdftocairo", "-png", "-singlefile", "-r", str(dpi), pdf, base],
                       check=True)
        im = Image.open(base + ".png").convert("RGB")
    bbox = ImageChops.difference(im, Image.new("RGB", im.size, (255, 255, 255))).getbbox()
    if bbox:
        im = im.crop(bbox)
    im.save(out)
    return out


def median_glyph_px(path):
    im = np.asarray(Image.open(path).convert("L"))
    h, w = im.shape[:2]
    lbl, _ = ndimage.label(im < 150)
    heights = [s[0].stop - s[0].start
               for s in ndimage.find_objects(lbl)
               if 4 <= s[0].stop - s[0].start <= 40 and 2 <= s[1].stop - s[1].start <= 60]
    if not heights:
        raise ValueError("no character-sized components found (is this a text figure?)")
    return float(np.median(heights)), w, h


def css_class(name):
    return re.sub(r"[^a-z0-9-]+", "-", name.lower()).strip("-") or "fig"


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("fig", help="figure image (png/jpg) or a single-figure pdf")
    ap.add_argument("--target", type=float, default=6.0,
                    help="target displayed glyph height in CSS px (default 6)")
    ap.add_argument("--dpi", type=int, default=300, help="rasterisation DPI for pdf input")
    ap.add_argument("--alt", default="DESCRIBE THE FIGURE", help="alt / aria-label text")
    ap.add_argument("--out", help="png path when rasterising a pdf (default: <pdf>.png)")
    a = ap.parse_args()

    path = a.fig
    if path.lower().endswith(".pdf"):
        out = a.out or os.path.splitext(path)[0] + ".png"
        try:
            path = rasterise_pdf(a.fig, a.dpi, out)
        except (OSError, subprocess.CalledProcessError) as e:
            sys.exit(f"pdftocairo failed: {e}")
        print(f"# rasterised {a.fig} -> {path}  (move it under docs/ and fix the src= below)\n")

    try:
        g, w, h = median_glyph_px(path)
    except (OSError, ValueError) as e:
        sys.exit(f"{path}: {e}")

    width = round(a.target * w / g)
    box_h = round(width * h / w)
    frac = box_h / PAGE_PX
    tall = frac > 0.55
    src = path if path.startswith("docs/") else f"docs/{os.path.basename(path)}"
    cls = css_class(os.path.splitext(os.path.basename(path))[0])

    print(f"# {os.path.basename(path)}: {w}x{h}px, median glyph {g:.1f}px "
          f"-> width={width} (text ~{a.target:.0f}px), box {width}x{box_h}px "
          f"(~{frac*100:.0f}% of a page)")
    print(f"# {'TALL -> use the split-across-pages pattern' if tall else 'short -> a plain <img> is fine'}\n")

    print("# ---- paste into README.md ----")
    if not tall:
        print(f'<p align="center">\n  <img src="{src}" width="{width}"\n'
              f'       alt="{a.alt}">\n</p>')
    else:
        print(f'<p align="center" class="screen-only-fig">\n  <img src="{src}" width="{width}"\n'
              f'       alt="{a.alt}">\n</p>')
        print('<!-- Print-only copy so this tall figure flows across PDF pages instead of\n'
              '     jumping whole to the next one (a raster <img> cannot be page-split);\n'
              '     invisible on screen/GitHub. -->')
        print(f'<div class="print-splitfig {cls}" role="img" aria-label="{a.alt}"></div>')
        print("\n# ---- add inside the @media print block of README.css "
              "(see the readme-figures skill for the one-time pattern) ----")
        print(f'  .print-splitfig.{cls} {{ width: {width}px; height: {box_h}px;\n'
              f'    background-image: url("{src}"); }}')


if __name__ == "__main__":
    main()
