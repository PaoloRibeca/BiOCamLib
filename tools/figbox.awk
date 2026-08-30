# figbox.awk -- rewrite every embedded figure into something a page break can
# fall inside, run by markdown-pdf between pandoc and the headless render.
#
# A raster <img> is replaced content and cannot be broken.  One that does not fit
# in what is left of a page moves whole to the next, leaving the remainder of the
# page it came from blank -- up to the figure's own height, which for a diagram
# is easily a third of a page -- and one taller than a page cannot be shown at
# all.  What is wanted instead is what a browser shows: the figure where the text
# reaches it, continuing over the boundary if that is where it falls.
#
# A BOX WITH THE IMAGE AS ITS BACKGROUND IS NOT ENOUGH, and this is the part
# worth knowing.  A fragmenter breaks a block between the things inside it, so a
# box whose height comes from a `height` and whose inside is empty offers no
# break at all: faced with one that does not fit, the renderer takes the break
# BEFORE it -- a break between siblings, which is always available and always
# preferred -- and the whole box moves, gap and all.  Filling it with spacers
# does not help either, the break before it still being the better one.
#
# So the figure is emitted as a STACK OF SIBLING BANDS, each a slice of the image
# shown through `background-position`.  The breaks between them are the same kind
# the renderer already prefers, so it uses them, and a figure now splits wherever
# it happens to land.  The residual gap is one band -- BAND pixels, below -- and
# nothing is sliced or re-encoded: every band carries the same background, which
# is named once in a generated rule so the data appears once however many bands
# there are.
function b64val(c,   i) { i = index(B64, c); return i ? i - 1 : 0 }
# The first 44 base64 characters carry the first 33 bytes, and a PNG states its
# size in bytes 16 to 23: an 8-byte signature, a 4-byte chunk length, the tag
# `IHDR`, then width and height, both big-endian.  Reading it here is what lets
# the document say nothing about the figure's size and lets nothing be missing.
function png_size(b64, wh,   i, v, n, b) {
  n = 0
  for (i = 1; i <= 44; i += 4) {
    v = b64val(substr(b64, i, 1)) * 262144 + b64val(substr(b64, i + 1, 1)) * 4096 \
      + b64val(substr(b64, i + 2, 1)) * 64 + b64val(substr(b64, i + 3, 1))
    b[n++] = int(v / 65536) % 256; b[n++] = int(v / 256) % 256; b[n++] = v % 256
  }
  wh[0] = ((b[16] * 256 + b[17]) * 256 + b[18]) * 256 + b[19]
  wh[1] = ((b[20] * 256 + b[21]) * 256 + b[22]) * 256 + b[23]
}
# The end of a tag, respecting quoted values: an aria-label is prose and may hold
# a `>`, which a match on /<img[^>]*>/ would cut the tag at.
function tag_end(s, from,   i, c, q) {
  for (i = from; i <= length(s); ++i) {
    c = substr(s, i, 1)
    if (c == "\"") q = !q
    else if (c == ">" && !q) return i
  }
  return 0
}
function attr(tag, name) {
  if (match(tag, name "=\"[^\"]*\""))
    return substr(tag, RSTART + length(name) + 2, RLENGTH - length(name) - 3)
  return ""
}
BEGIN {
  RS = "\0"
  B64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  # The granularity at which a figure may break, and so also the largest gap one
  # can leave: about a line of text, which is below noticing on a printed page.
  BAND = 16
  PREFIX = "data:image/png;base64,"
}
{
  out = ""; rest = $0; rules = ""; nfig = 0
  while (match(rest, /<img[ \t\n]/)) {
    start = RSTART; stop = tag_end(rest, start)
    if (!stop) break
    tag = substr(rest, start, stop - start + 1)
    out = out substr(rest, 1, start - 1); rest = substr(rest, stop + 1)
    src = attr(tag, "src")
    # Anything that is not an embedded PNG is left exactly as it was: a figure
    # this cannot size is better shown whole and unbreakable than not shown.
    if (index(src, PREFIX) != 1) { out = out tag; continue }
    png_size(substr(src, length(PREFIX) + 1), wh)
    if (wh[0] <= 0 || wh[1] <= 0) { out = out tag; continue }
    w = attr(tag, "width") + 0
    if (w <= 0) w = wh[0]
    h = w * wh[1] / wh[0]
    label = attr(tag, "aria-label")
    if (label == "") label = attr(tag, "alt")
    gsub(/"/, "\\&quot;", label)
    id = "pdf-fig-" ++nfig
    # The image is named ONCE, here, however many bands refer to it.
    rules = rules sprintf("\n.%s span { background-image: url(\"%s\"); background-size: %dpx auto; }",
                          id, src, w)
    rows = int(h / BAND); if (rows < 1) rows = 1
    bh = h / rows
    bands = ""
    for (r = 0; r < rows; ++r)
      bands = bands sprintf("<span style=\"height:%.4fpx;background-position:0 -%.4fpx\"></span>",
                            bh, r * bh)
    out = out sprintf("<span class=\"pdf-fig %s\" role=\"img\" aria-label=\"%s\"" \
                      " style=\"width:%dpx\">%s</span>", id, label, w, bands)
  }
  doc = out rest
  if (rules != "") {
    i = index(doc, "</head>")
    style = "<style>" rules "\n</style>\n"
    if (i) doc = substr(doc, 1, i - 1) style substr(doc, i)
    else doc = style doc
  }
  printf "%s", doc
}
