(*
    Plot.ml -- (c) 2026 Paolo Ribeca, <paolo.ribeca@gmail.com>

    This file is part of BiOCamLib, the OCaml foundations upon which
    a number of the bioinformatics tools I developed are built.

    Plot.ml implements simple vector plots -- a dot plot and a depth
    track -- rendered to a multi-page PDF through the Vg library, so that
    tools can produce their own reports without calling out to R or
    ghostscript.  Vg and Gg are opened only inside the encapsulated
    struct, and a page is kept abstract, so a caller never names a Vg
    type.  Plots are authored in Vg's native y-up coordinates (origin at
    the bottom left), which keeps text upright with no final flip.

    This program was designed and developed by the author(s),
    with the assistance of the following AI tool(s):
      2026 Claude (Anthropic).
    The final logic and implementation were reviewed and verified in
    their entirety by the author(s).

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

include (
  struct
    open Gg
    open Vg
    (* A page is its physical size in mm and the image drawn on it. *)
    type page = Gg.size2 * Vg.image
    (* Colours (sRGB). *)
    let black = Color.v_srgb 0. 0. 0.
    and blue = Color.v_srgb 0. 0. 0.8
    and red = Color.v_srgb 0.85 0. 0.
    and green = Color.v_srgb 0. 0.6 0.
    and grey = Color.v_srgb 0.5 0.5 0.5
    (* Round caps and joins so that the abutting segments of a border or an
       axis meet cleanly rather than leaving square corners. *)
    let stroke ~width = `O { P.o with P.width; cap = `Round; join = `Round }
    let line ~x1 ~y1 ~x2 ~y2 ?(width = 0.2) color =
      let p = P.empty |> P.sub (P2.v x1 y1) |> P.line (P2.v x2 y2) in
      I.const color |> I.cut ~area:(stroke ~width) p
    (* An open polyline through the points, used for the depth trace. *)
    let polyline points ?(width = 0.2) color =
      match points with
      | [] | [_] -> I.void
      | (x0, y0) :: rest ->
        let p =
          List.fold_left (fun p (x, y) -> P.line (P2.v x y) p)
            (P.empty |> P.sub (P2.v x0 y0)) rest in
        I.const color |> I.cut ~area:(stroke ~width) p
    (* Standard-PDF Helvetica: with an empty glyph list Vgr_pdf lays the text
       out from the utf-8 string itself, so no font need be embedded.  The text
       is rotated around its anchor, then moved to (x, y). *)
    let text ~x ~y ?(size = 4.) ?(rotate = 0.) color s =
      let font = { Font.name = "Helvetica"; slant = `Normal; weight = `W400; size } in
      let glyphs = I.const color |> I.cut_glyphs ~text:s font [] in
      (if rotate = 0. then glyphs else I.rot rotate glyphs) |> I.move (P2.v x y)
    (* A rough Helvetica advance width, for centring labels without measuring. *)
    let text_width s size = float_of_int (String.length s) *. size *. 0.55
    (* Later images are composited on top, so the list is drawn back to front. *)
    let stack images = List.fold_left (fun under over -> I.blend over under) I.void images
    (* At most eleven ticks: the step is the smallest 1/2/5*10^k that is at least
       the range over ten, so there are never more than ten intervals, and none
       falls past the range. *)
    let nice_ticks hi =
      if hi <= 0. then [0.]
      else begin
        let raw = hi /. 10. in
        let mag = 10. ** Float.floor (Float.log10 raw) in
        let norm = raw /. mag in
        let step = (if norm <= 1. then 1. else if norm <= 2. then 2. else if norm <= 5. then 5. else 10.) *. mag in
        let rec loop x acc = if x > hi then List.rev acc else loop (x +. step) (x :: acc) in
        loop 0. []
      end
    (* A tick label: a plain number below a thousand, otherwise kb or Mb with at
       most one decimal (integer where it lands on one).  The unit is chosen once
       from the axis extent, so the whole axis reads in the same unit. *)
    let tick_label ~extent v =
      let factor, suffix =
        if extent < 1000. then 1., ""
        else if extent < 1_000_000. then 1000., " kb"
        else 1_000_000., " Mb" in
      let x = v /. factor in
      if Float.equal (Float.round x) x then Printf.sprintf "%d%s" (int_of_float x) suffix
      else Printf.sprintf "%.1f%s" x suffix
    (* Left and bottom carry the tick labels and the axis title; top and right
       carry the sequence names, which reach further from the box, so the two
       margins differ and the tick side stays tight against the axis. *)
    let margin_lo = 15. and margin_hi = 30.
    (* Text sizes in mm, kept large and legible, in the manner of R's defaults. *)
    let tick_size = 4.5 and title_size = 6. and name_size = 3.5
    (* Tick labels sit just outside the box: x centred below its axis, y rotated
       and centred to the axis's left, both the same short gap from the axis. *)
    let x_tick_label ~x ~axis ~extent v =
      let s = tick_label ~extent v in
      text ~x:(x -. text_width s tick_size /. 2.) ~y:(axis -. 5.5) ~size:tick_size black s
    let y_tick_label ~y ~axis ~extent v =
      let s = tick_label ~extent v in
      text ~x:(axis -. 3.5) ~y:(y -. text_width s tick_size /. 2.) ~size:tick_size ~rotate:(Float.pi /. 2.) black s
    (* The dot plot: the concatenated query on x, the concatenated target on y,
       each alignment a blue segment (a reverse match runs on the anti-diagonal
       because its target coordinates were already swapped by the caller).  A
       fixed square frame, closed in black; each axis fills it, so a colinear
       alignment runs corner to corner whatever the two lengths.  Sequence
       boundaries are red, and everything -- ticks, sequence names, axis titles
       -- sits outside the box: the x names rotated above it, the y names to its
       right. *)
    let dotplot ~x_label ~y_label ~x_seqs ~y_seqs ~segments =
      let total l = List.fold_left (fun acc (_, n) -> acc + n) 0 l in
      let sum_x = float_of_int (total x_seqs) and sum_y = float_of_int (total y_seqs) in
      let box = 160. in
      let scale_x = box /. Float.max sum_x 1. and scale_y = box /. Float.max sum_y 1. in
      let ox = margin_lo and oy = margin_lo in
      let right = ox +. box and top = oy +. box in
      let sx q = ox +. float_of_int q *. scale_x and sy t = oy +. float_of_int t *. scale_y in
      let frame =
        [ line ~x1:ox ~y1:oy ~x2:right ~y2:oy black;
          line ~x1:ox ~y1:top ~x2:right ~y2:top black;
          line ~x1:ox ~y1:oy ~x2:ox ~y2:top black;
          line ~x1:right ~y1:oy ~x2:right ~y2:top black ] in
      let x_marks =
        let cum = ref 0 and acc = ref [] in
        List.iter
          (fun (name, n) ->
            if !cum > 0 then acc := line ~x1:(sx !cum) ~y1:oy ~x2:(sx !cum) ~y2:top red :: !acc;
            acc := text ~x:(sx !cum +. 2.) ~y:(top +. 2.) ~size:name_size ~rotate:(Float.pi /. 2.) grey name :: !acc;
            cum := !cum + n)
          x_seqs;
        !acc
      and y_marks =
        let cum = ref 0 and acc = ref [] in
        List.iter
          (fun (name, n) ->
            if !cum > 0 then acc := line ~x1:ox ~y1:(sy !cum) ~x2:right ~y2:(sy !cum) red :: !acc;
            acc := text ~x:(right +. 2.) ~y:(sy !cum -. name_size *. 0.35) ~size:name_size grey name :: !acc;
            cum := !cum + n)
          y_seqs;
        !acc in
      let x_ticks =
        List.concat_map
          (fun t ->
            [ line ~x1:(sx (int_of_float t)) ~y1:(oy -. 2.) ~x2:(sx (int_of_float t)) ~y2:oy black;
              x_tick_label ~x:(sx (int_of_float t)) ~axis:oy ~extent:sum_x t ])
          (nice_ticks sum_x)
      and y_ticks =
        List.concat_map
          (fun t ->
            [ line ~x1:(ox -. 2.) ~y1:(sy (int_of_float t)) ~x2:ox ~y2:(sy (int_of_float t)) black;
              y_tick_label ~y:(sy (int_of_float t)) ~axis:ox ~extent:sum_y t ])
          (nice_ticks sum_y) in
      let segs =
        List.map
          (fun (lo_q, hi_q, lo_t, hi_t) -> line ~x1:(sx lo_q) ~y1:(sy lo_t) ~x2:(sx hi_q) ~y2:(sy hi_t) ~width:0.4 blue)
          segments in
      let titles =
        [ text ~x:(ox +. box /. 2. -. text_width x_label title_size /. 2.) ~y:(oy -. 12.) ~size:title_size black x_label;
          text ~x:6. ~y:(oy +. box /. 2. -. text_width y_label title_size /. 2.) ~size:title_size ~rotate:(Float.pi /. 2.) black y_label ] in
      let image = stack (List.concat [ segs; x_marks; y_marks; frame; x_ticks; y_ticks; titles ]) in
      (Size2.v (margin_lo +. box +. margin_hi) (margin_lo +. box +. margin_hi), image)
    (* The depth track: per-base depth along the concatenated sequences, drawn
       as a blue trace on a log2(depth + 1) y by default, whose range runs from 0
       to the deepest position's transformed value.  A bedgraph is a list of
       (name, start, end, value) intervals; the x axis counts only covered bases,
       so a within-sequence gap in the bedgraph closes up and is marked green,
       while a change of sequence is marked red.  The box is closed in black and
       the sequence names sit rotated above it. *)
    let depth ~label ?(logarithmic = true) records =
      let draw_w = 240. and draw_h = 120. in
      let ox = margin_lo and oy = margin_lo in
      let right = ox +. draw_w and top = oy +. draw_h in
      let values = ref [] and marks = ref [] and max_v = ref 0. in
      let old_name = ref "" and old_end = ref 0 and acc = ref 0 in
      List.iter
        (fun (name, lo, hi, v) ->
          if name <> !old_name then
            marks := (!acc, red, name) :: !marks
          else if lo <> !old_end then
            marks := (!acc, green, name) :: !marks;
          for _ = lo to hi - 1 do
            values := v :: !values;
            if v > !max_v then max_v := v
          done;
          old_name := name;
          old_end := hi;
          acc := !acc + hi - lo)
        records;
      let values = Array.of_list (List.rev !values) in
      let n = Array.length values in
      let log2 x = log x /. log 2. in
      let y_extent = if logarithmic then log2 (1. +. !max_v) else !max_v in
      let transform v = if logarithmic then log2 (1. +. v) else v in
      let sy_pos p = oy +. (if y_extent <= 0. then 0. else p /. y_extent *. draw_h) in
      let sy v = sy_pos (transform v) in
      let sx i = ox +. (if n <= 1 then 0. else float_of_int i /. float_of_int (n - 1) *. draw_w) in
      let trace =
        let pts = ref [] in
        for i = n - 1 downto 0 do pts := (sx i, sy values.(i)) :: !pts done;
        polyline !pts ~width:0.3 blue in
      let boundaries =
        List.concat_map
          (fun (i, color, name) ->
            [ line ~x1:(sx i) ~y1:oy ~x2:(sx i) ~y2:top color;
              text ~x:(sx i +. 2.) ~y:(top +. 2.) ~size:name_size ~rotate:(Float.pi /. 2.) grey name ])
          (List.rev ((n, red, "") :: !marks)) in
      let frame =
        [ line ~x1:ox ~y1:oy ~x2:right ~y2:oy black;
          line ~x1:ox ~y1:top ~x2:right ~y2:top black;
          line ~x1:ox ~y1:oy ~x2:ox ~y2:top black;
          line ~x1:right ~y1:oy ~x2:right ~y2:top black ] in
      let x_ticks =
        List.concat_map
          (fun t ->
            [ line ~x1:(sx (int_of_float t)) ~y1:(oy -. 2.) ~x2:(sx (int_of_float t)) ~y2:oy black;
              x_tick_label ~x:(sx (int_of_float t)) ~axis:oy ~extent:(float_of_int n) t ])
          (nice_ticks (float_of_int n))
      and y_ticks =
        List.concat_map
          (fun p ->
            [ line ~x1:(ox -. 2.) ~y1:(sy_pos p) ~x2:ox ~y2:(sy_pos p) black;
              y_tick_label ~y:(sy_pos p) ~axis:ox ~extent:y_extent p ])
          (nice_ticks y_extent) in
      let titles =
        [ text ~x:(ox +. draw_w /. 2. -. text_width label title_size /. 2.) ~y:(oy -. 12.) ~size:title_size black label;
          text ~x:6. ~y:(oy +. draw_h /. 2. -. 24.) ~size:title_size ~rotate:(Float.pi /. 2.) black
            (if logarithmic then "log2(depth+1)" else "depth") ] in
      let image =
        if n = 0 then text ~x:(ox +. draw_w /. 2.) ~y:(oy +. draw_h /. 2.) ~size:8. grey "no reads"
        else stack (List.concat [ [ trace ]; boundaries; frame; x_ticks; y_ticks; titles ]) in
      (Size2.v (2. *. margin_lo +. draw_w) (margin_lo +. draw_h +. margin_hi), image)
    (* Arrange sub-pages into a grid of the given number of columns on a fixed
       square canvas, each scaled to fit a uniform cell and centred in it, so
       that montages with different grids still share the same page size. *)
    let montage ~columns pages =
      let n = List.length pages in
      let columns = max 1 columns in
      let rows = max 1 ((n + columns - 1) / columns) in
      let canvas = 520. and gap = 6. in
      let cell_w = (canvas -. float_of_int (columns + 1) *. gap) /. float_of_int columns
      and cell_h = (canvas -. float_of_int (rows + 1) *. gap) /. float_of_int rows in
      let placed =
        List.mapi
          (fun i (sz, img) ->
            let col = i mod columns and row = i / columns in
            let s = Float.min (cell_w /. Size2.w sz) (cell_h /. Size2.h sz) in
            let cw = Size2.w sz *. s and ch = Size2.h sz *. s in
            let cx = gap +. float_of_int col *. (cell_w +. gap) +. (cell_w -. cw) /. 2. in
            let cy = canvas -. gap -. float_of_int row *. (cell_h +. gap) -. (cell_h +. ch) /. 2. in
            img |> I.scale (V2.v s s) |> I.move (P2.v cx cy))
          pages in
      (Size2.v canvas canvas, stack placed)
    (* Vgr_pdf renders one page per renderable, so the whole report is one
       create, N images, one close -- ghostscript concatenation is not needed. *)
    let to_pdf path pages =
      let oc = open_out path in
      let r = Vgr.create (Vgr_pdf.target ()) (`Channel oc) in
      List.iter
        (fun (size_mm, image) ->
          let view = Box2.v (P2.v 0. 0.) size_mm in
          ignore (Vgr.render r (`Image (size_mm, view, image))))
        pages;
      ignore (Vgr.render r `End);
      close_out oc
  end: sig
    type page
    val dotplot:
      x_label:string -> y_label:string ->
      x_seqs:(string * int) list -> y_seqs:(string * int) list ->
      segments:(int * int * int * int) list -> page
    val depth: label:string -> ?logarithmic:bool -> (string * int * int * float) list -> page
    val montage: columns:int -> page list -> page
    val to_pdf: string -> page list -> unit
  end
)
