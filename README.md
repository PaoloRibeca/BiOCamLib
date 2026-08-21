# BiOCamLib

BiOCamLib is the [OCaml](https://ocaml.org) foundation upon which a number of the bioinformatics tools I developed are built.

It mostly consists of a library &mdash; you'll need to clone this repository if you want to manually compile other programs I've developed, notably [SiNPle](https://github.com/PaoloRibeca/SiNPle) or [KPop](https://github.com/PaoloRibeca/KPop). You might also use the library for your own programs, if you are familiar with OCaml and patient enough to read the code.

As a bonus, BiOCamLib comes bundled with a few programs:
* `RC`, which can efficiently compute the reverse complement of (possibly very long) sequences. Each sequence should be input on a separate line &mdash; lines are processed one by one and not buffered. I use this program in many of my workflows.
* `Octopus`, which is a high-throughput program to compute the transitive closure of strings. This is useful to cluster things.
* `Parallel`, which allows you to split and process an input file chunk-wise using the reader/workers/writer model implemented in `BiOCamLib.Tools.Parallel`. You can see it as a demonstration of the capabilities of the library, but I also often use it as a useful tool to solve real-life problems, be they bioinformatics or not. A number of high-throughput real-life examples can be found in the [KPop README](https://github.com/PaoloRibeca/KPop/).
* `FASTools`, which is a Swiss-knife tool for the manipulation of FASTA/FASTQ files. It supports all formats (FASTA, single- and paired-end FASTQ, interleaved FASTQ) and a simpler tabular format whereby FASTA/FASTQ records are represented as tab-separated lines. It facilitates format interconversions and other manipulations.
* `TREx`, which finds exact tandem repeats in all the sequences of a FASTA file. The output is a tab-separated table of repeats with their position, length, and unit, suitable for downstream filtering or annotation.
* `Cophenetic`, which reads a Newick tree on standard input and writes the cophenetic-distance matrix between every pair of labelled nodes (leaves and internal alike) to standard output. By default the matrix carries shortest-path distances along branch lengths; an option switches to longest-path distances, which is the right convention when branch lengths encode partial quantities (e.g. allele frequencies).
* `Yggdrasill`, which builds a phylogenetic tree from a register of weighted splits. It loads a `.PhyloSplits` file (binary or tabular), greedy-filters the splits for pairwise compatibility in order of decreasing weight, and emits a Newick tree assembled via the Buneman construction. The incompatible residual is retained for further inspection or iterative refinement, and a dropped-weight ratio is emitted to stderr as a measure of how tree-like the input split system is. This is the natural downstream tool for [KPop](https://github.com/PaoloRibeca/KPop/)'s `KPopTwistDB --splits` output.
* `AnnoTools`, which manipulates a single in-memory genome-annotation register through a CLI-driven action stream. It can read and write GFF3, GTF, and GenBank files (round-tripping each), attach a multi-FASTA reference, validate that an annotation is consistent with the reference, select features and extract their DNA or protein sequences, and serialise the register either to a compact binary `.Annotation` archive that loads orders of magnitude faster than reparsing the source, or to a lossless set of tab-separated tables you can `diff`, `sort` and `join` with the rest of a Unix pipeline.

## Installing `RC`, `Octopus`, `Parallel`, `FASTools`, `TREx`, `Cophenetic`, `Yggdrasill`, and `AnnoTools`

:warning: Note that the only operating systems we officially support are Linux and MacOS. :warning:

There are several possible ways of installing the software on your machine: through `conda`; by downloading pre-compiled binaries (Linux and MacOS x86_64 only); or manually.

### Conda channel

BiOCamLib can be installed from the `bioconda` channel as package `biocamlib`. Just type
```bash
conda install -c bioconda biocamlib
```
or the equivalent command obtained replacing `conda` with `mamba` or `micromamba`, depending on which program you habitually use.

### Pre-compiled binaries

You can download pre-compiled binaries for Linux and MacOS x86_64 from our [releases](https://github.com/PaoloRibeca/BiOCamLib/releases). After doing so, just copy or move them to a directory which is accessible from your PATH.

For instance, supposing that you've downloaded programs to directory `~/.local/bin/`, in order to make them accessible from everywhere you'll have to execute a command such as
```bash
export PATH=~/.local/bin:$PATH
```
or add it to one of your login scripts (such as `~/.bashrc` or similar for `bash`).

Note that the binaries are generated according to the recipe described [here](https://github.com/PaoloRibeca/ocaml-static-binaries).

### Manual install

Alternatively, you can install `RC`, `Octopus`, `Parallel`, `FASTools`, `TREx`, `Cophenetic`, `Yggdrasill`, and `AnnoTools` manually by cloning and compiling the BiOCamLib sources. You'll need an up-to-date distribution of the OCaml compiler and the [Dune package manager](https://github.com/ocaml/dune) for that. Both can be installed through [OPAM](https://opam.ocaml.org/), the official OCaml distribution system. Once you have a working OPAM distribution you'll also have a working OCaml compiler, and Dune can be installed with the command
```bash
opam install dune
```
if it is not already present. Make sure that you install OCaml version 4.12 or later.

Then go to the directory into which you have downloaded the latest BiOCamLib sources, and type
```bash
./BUILD
```

This should generate the executables `RC`, `Octopus`, `Parallel`, `FASTools`, `TREx`, `Cophenetic`, `Yggdrasill`, and `AnnoTools`. Copy them to some favourite location in your PATH, for instance `~/.local/bin`.

## Using `RC`

`RC` inputs sequences from standard input and outputs their reverse complement to standard output, one sequence at the time. Hence, `RC` can be conveniently used in a subprocess. For example, the command
```bash
echo GAtTaCA | RC
```
would produce `TGtAaTC`. The whole IUPAC nucleotide alphabet is complemented, not just `[ACGTacgt]` &mdash; an ambiguity code is mapped to the code for the complementary set, so `R` (`A`/`G`) becomes `Y` (`C`/`T`), `B` becomes `V`, and the self-complementary `S`, `W` and `N` are left alone. `U` complements to `A`. Case is always preserved. Characters outside the nucleotide alphabet, gaps included, are output unmodified, so sequence validation and linting must still be performed elsewhere whenever they are necessary.

### Command line options

This is the full list of command line options available for the program `RC`. You can visualise the list by typing
```bash
RC -h
```
in your terminal. You will see a header containing information about the version:
```
This is RC version 3 [02-Jan-2024]
 compiled against: BiOCamLib version 242 [23-Jan-2024]
 (c) 2023-2024 Paolo Ribeca <paolo.ribeca@gmail.com>
```
followed by detailed information. The general form(s) the command can be used is:
```
RC [OPTIONS]
```

**Algorithm**

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `-C`<br>`--no-complement` |  |  do not base\-complement the sequence | <ins>default=<mark>_base\-complement_</mark></ins> |

**Miscellaneous**

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `-V`<br>`--version` |  |  print version and exit |  |
| `-h`<br>`--help` |  |  print syntax and exit |  |

## Using `Octopus`

`Octopus` reads from its standard input equivalence relations, one set of relations per line. Each line consists of a set of strings separated by whitespace; if any two labels appear on the same line, they are considered to belong to the same equivalence class. When all the input has been parsed, `Octopus` outputs all the labels seen in the input sorted according to their equivalence class &mdash; each line contains one equivalence class, with its member string labels separated by a `\t` character. The order in which classes appear is kept, but elements within the class will be lexicographically sorted. For example, the command
```bash
(cat <<___
A duh
  b  C
c f e
 duh zz x
b c
___
) | Octopus
```
will result in the output
```
A       duh     x       zz
C       b       c       e       f
```
(tab-separated).

### Command line options for `Octopus`

This is the full list of command line options available for the program `Octopus`. You can visualise the list by typing
```bash
Octopus -h
```
in your terminal. You will see a header containing information about the version:

```
This is Octopus version 6 [02-Jan-2024]
 compiled against: BiOCamLib version 242 [23-Jan-2024]
 (c) 2016-2024 Paolo Ribeca <paolo.ribeca@gmail.com>
```
followed by detailed information. The general form(s) the command can be used is:
```
Octopus [OPTIONS]
```

**Miscellaneous**

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `-V`<br>`--version` |  |  print version and exit |  |
| `-h`<br>`--help` |  |  print syntax and exit |  |

## Using `Parallel`

`Parallel` implements a subset of the functionality of [GNU `parallel`](https://www.gnu.org/software/parallel/), but is a compiled binary (hence potentially faster) and does not require you to install PERL or any other dependencies. It also has a much simpler interface. You can use it to transparently split a multi-line file into blocks having a specified size, which are then fed as standard input to a pool of worker threads. The number of threads is specified by the user, and the next block to be processed is assigned to the first thread that becomes available. The results of the processing of the blocks are concatenated in the order of the original blocks. If the number of threads is not specified, as many are used as the number of available processing units/CPU cores (as determined by the `nproc` command).

A typical bioinformatic use case might be something like
```bash
cat LargeFile.fasta | Parallel -l 1000 -- awk '{if ($0~"^>") print; else print toupper($0)}'
```
which would spawn as many workers as the number of available cores, split the input FASTA file into blocks of 1,000 lines each and process each block through `awk` to capitalise the sequence.

Keep in mind that there is little point in parallelising a job if sending/receiving data to/from it takes more time than the computation itself!

### Command line options for `Parallel`

This is the full list of command line options available for the program `Parallel`. You can visualise the list by typing
```bash
Parallel -h
```
in your terminal. You will see a header containing information about the version:
```
This is Parallel version 8 [18-Jan-2024]
 compiled against: BiOCamLib version 242 [23-Jan-2024]
 (c) 2019-2024 Paolo Ribeca <paolo.ribeca@gmail.com>
```
followed by detailed information. The general form(s) the command can be used is:
```
Parallel [OPTIONS] -- [COMMAND TO PARALLELIZE AND ITS OPTIONS]
```

**Command to parallelize**

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `--` |  |  consider all the subsequent parameters as the command to be executed in parallel\.<br>At least one command must be specified | *(mandatory)* |

**Input/Output**

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `-l`<br>`--lines-per-block` | _positive\_integer_ |  number of lines to be processed per block | <ins>default=<mark>_10000_</mark></ins> |
| `-i`<br>`--input` | _input\_file_ |  name of input file | <ins>default=<mark>_stdin_</mark></ins> |
| `-o`<br>`--output` | _output\_file_ |  name of output file | <ins>default=<mark>_stdout_</mark></ins> |

**Miscellaneous**

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `-t`<br>`--threads` | _positive\_integer_ |  number of concurrent computing threads to be spawned  \(default automatically detected from your configuration\) | <ins>default=<mark>_nproc_</mark></ins> |
| `-v`<br>`--verbose` |  |  set verbose execution | <ins>default=<mark>_false_</mark></ins> |
| `-d`<br>`--debug` |  |  output debugging information | <ins>default=<mark>_false_</mark></ins> |
| `-V`<br>`--version` |  |  print version and exit |  |
| `-h`<br>`--help` |  |  print syntax and exit |  |

## Using `FASTools`

`FASTools` allows you to manipulate FASTA/FASTQ files by offering a rich set of tools to convert FASTA/FASTQ records to/from a tab-separated format. Together with other programs such as `awk`, that allows a large set of operations to be implemented effortlessly.

For example:
```bash
cat Sequences.fasta | FASTools | awk -F '\t' '{print ">"$1"\n"substr($2,1,int(length($2)+1)/2)}'
```
puts each FASTA record in file `Sequences.fasta` on a single line, with the sequence name and the sequence being separated by a `'\t'` character. The `awk` part then reads each line, replaces the sequence with its first half, and re-outputs the sequence as a FASTA record.

Due to default command line option values, the command is actually equivalent to
```bash
cat Sequences.fasta | FASTools c -F | awk -F '\t' '{print ">"$1"\n"substr($2,1,int(length($2)+1)/2)}'
```
and, if one uses forms accepting a direct rather than a piped input, to
```bash
FASTools -f Sequences.fasta | awk -F '\t' '{print ">"$1"\n"substr($2,1,int(length($2)+1)/2)}'
```
or to
```bash
FASTools c -f Sequences.fasta | awk -F '\t' '{print ">"$1"\n"substr($2,1,int(length($2)+1)/2)}'
```

Similar command line options are provided to manipulate as single lines FASTQ and tabular formats rather than FASTA. In detail:
* Option `-f <FASTA_file>` operates on a FASTA file given as an explicit argument, while `-F` processes the same kind of file piped into the program as standard input
* Option `-s <FASTQ_file>` operates on a FASTQ file containing single-end reads given as an explicit argument, while `-S` processes the same kind of file piped into the program as standard input
* Option `-p <FASTQ_file_1> <FASTQ_file_2>` operates on two FASTQ file separately containing first and second ends of paired-end reads given as an explicit argument, while `-P` processes an interleaved FASTQ file containing alternating first and second ends of paired-end reads piped into the program as standard input
* Option `-t <tabular_file>` operates on a tabular file containing FASTA/FASTQ records compacted on a single line by FASTools given as an explicit argument, while `-T` processes the same kind of file piped into the program as standard input.
  Note that there are exactly three possible tabular formats recognised by FASTools:
  1. Lines of the form `<read_name> '\t' <read_sequence>`, corresponding to a compacted FASTA record
  2. Lines of the form `<read_name> '\t' <read_sequence> '\t' <read_qualities>`, corresponding to a compacted single-end FASTQ record
  3. Lines of the form `<read_name_1> '\t' <read_sequence_1> '\t' <read_qualities_1> '\t' <read_name_2> '\t' <read_sequence_2> '\t' <read_qualities_2>`, corresponding to a compacted paired-end FASTQ record.

Several command switches exist that natively perform operations on sequence/qualities within `FASTools`, with no need to write any additional external code:
* Option `c` or `-c`, a shorthand for `compact`, will group each FASTA/FASTQ record on a single tab-separated line. It is the default
* Option `e` or `-e`, a shorthand for `expand`, will split tabular records into multi-line FASTA/FASTQ records. It is the inverse of `c`. Note that it is perfectly fine to apply option `e` to an existing input which is already in FASTA/FASTQ format, in which case `FASTools` will normalise the input by putting each sequence on a single line, which is how `FASTools` outputs sequences. Linting filters (option `-l`) can also be used to further normalise the sequence
* Option `m <regexp>` or `-m <regexp>`, a shorthand for `match <regexp>`, will select FASTA/FASTQ records whose name match '<regexp>'. Note that `<regexp>` must be defined according to [https://ocaml.org/api/Str.html](https://ocaml.org/api/Str.html)
* Option `r` or `-r`, a shorthand for `revcom`, will reverse-complement the sequence, and reverse the qualities if present, of FASTA/FASTQ records
* Option `d` or `-d`, a shorthand for `dropq`, will drop qualities from FASTA/FASTQ records, turning a FASTQ into a FASTA file and having no effect on a FASTA file.

Repeated command line options can be seen as different commands that are executed in order of specification. For instance,
```bash
FASTools m "^CONTIG_1$" -f Assembly_1.fasta m "^CONTIG_42$" -f Assembly_2.fasta
```
will select sequence `CONTIG_1` from file `Assembly_1.fasta` and sequence `CONTIG_42` from file `Assembly_2.fasta`, outputting one after the other. Note however that each `c`/`e`/`m`/`r`/`d` option will input and output precisely one file, so option `c` in
```bash
FASTools e -f Multiline.fasta c
```
will not have any effect &mdash; the result will be a FASTA file with sequences on the same line, not a tabular one. To perform on the same input more than one transformation requiring I/O, you'll have to pipe multiple `FASTools` commands one after the other, as in
```bash
FASTools e -f Multiline.fasta | FASTools c
```
which will give the expected result.

A few more examples:
* ```bash
  FASTools e -p <(zcat Sample76_1.fq.gz) <(zcat Sample76_2.fq.gz)
  ```
  will interleave compressed files, while
* ```bash
  FASTools -s Reads.fastq | shuf | awk '((NR-1)%10==0)' | FASTools e -T
  ```
  will randomly select one read in 10. Note that the corresponding version with paired-end files in input, namely:
* ```bash
  FASTools -p Reads_1.fastq Reads_2.fastq | shuf | awk '((NR-1)%10==0)' | FASTools e -T
  ```
  will still work correctly and produce an interleaved output.

Finally, you can combine `FASTools` with [`Parallel`](#using-parallel) to distribute computation across different CPUs. For instance, the command:
```bash
FASTools -f Sequences.fasta | Parallel -- awk -F '\t' '{print ">"$1"\n"substr($2,1,int(length($2)+1)/2)}'
```
will still cut sequences in half as the first example, but it will do so by splitting the input file compacted by FASTools into blocks of 10,000 records each and by distributing the computation among all the available CPU cores.

### Command line options for `FASTools`

This is the full list of command line options available for the program `FASTools`. You can visualise the list by typing
```bash
FASTools -h
```
in your terminal. You will see a header containing information about the version:
```
This is FASTools version 8 [18-Mar-2024]
 compiled against: BiOCamLib version 245 [14-Feb-2024]
 (c) 2022-2024 Paolo Ribeca <paolo.ribeca@gmail.com>
```
followed by detailed information. The general form(s) the command can be used is:
```
FASTools [OPTIONS]
```

**Working mode.**
Executed delayed in order of specification, default=<mark>_compact_</mark>.

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `compact`<br>`-c`<br>`--compact` |  |  put each FASTA/FASTQ record on one tab\-separated line |  |
| `expand`<br>`-e`<br>`--expand` |  |  split each tab\-separated line into one or more FASTA/FASTQ records |  |
| `match`<br>`-m`<br>`--match` | _regexp_ |  select sequence names matching the specified regexp in FASTA/FASTQ records or tab\-separated lines\.<br>The regexp must be defined according to [https://ocaml.org/api/Str.html](https://ocaml.org/api/Str.html).<br>For paired\-end files, the pair matches when at least one name matches |  |
| `revcom`<br>`-r`<br>`--revcom` |  |  reverse\-complement sequences (and reverse qualities if present) in FASTA/FASTQ records or tab\-separated lines |  |
| `dropq`<br>`-d`<br>`--dropq` |  |  drop qualities in FASTA/FASTQ records or tab\-separated lines |  |

**Input/Output.**
Executed delayed in order of specification, default=<mark>_-F_</mark>.

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `-f`<br>`--fasta` | _fasta\_file\_name_ |  process FASTA input file containing sequences |  |
| `-F` |  |  process FASTA sequences from standard input |  |
| `-s`<br>`--single-end` | _fastq\_file\_name_ |  process FASTQ input file containing single\-end sequencing reads |  |
| `-S` |  |  process single\-end FASTQ sequencing reads from standard input |  |
| `-p`<br>`--paired-end` | _fastq\_file\_name1 fastq\_file\_name2_ |  process FASTQ input files containing paired\-end sequencing reads |  |
| `-P` |  |  process interleaved FASTQ sequencing reads from standard input |  |
| `-t`<br>`--tabular` | _tabular\_file\_name_ |  process input file containing FAST\[A&#124;Q\] records as tab\-separated lines |  |
| `-T` |  |  process FAST\[A&#124;Q\] records in tabular form from standard input |  |
| `-l`<br>`--linter` | `none` _&#124;_ `DNA` _&#124;_ `dna` _&#124;_ `protein` |  sets linter for sequence\.<br>All non\-base \(for DNA\) or non\-AA \(for protein\) characters  are converted to unknowns | <ins>default=<mark>_none_</mark></ins> |
| `--linter-keep-lowercase` | `true` _&#124;_ `false` |  sets whether the linter should keep lowercase DNA/protein characters  appearing in sequences rather than capitalise them | <ins>default=<mark>_false_</mark></ins> |
| `--linter-keep-dashes` | `true` _&#124;_ `false` |  sets whether the linter should keep dashes appearing in sequences  rather than convert them to unknowns | <ins>default=<mark>_false_</mark></ins> |
| `-o`<br>`--output` | _output\_file\_name_ |  set the name of the output file\.<br>Files are kept open, and it is possible to switch between them  by repeatedly using this option\.<br>Use `/dev/stdout` for standard output | <ins>default=<mark>_/dev/stdout_</mark></ins> |
| `-O`<br>`--paired-end-output` | _output\_file\_name\_1 output\_file\_name\_2_ |  set the names of paired\-end FASTQ output files\.<br>Files are kept open, and it is possible to switch between them  by repeatedly using this option\.<br>Use `/dev/stdout` for standard output | <ins>default=<mark>_/dev/stdout_</mark></ins> |
| `--flush`<br>`--flush-output` |  |  flush output after each record \(global option\) | <ins>default=<mark>_do not flush_</mark></ins> |

**Miscellaneous**

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `-v`<br>`--verbose` |  |  set verbose execution \(global option\) | <ins>default=<mark>_false_</mark></ins> |
| `-V`<br>`--version` |  |  print version and exit |  |
| `-h`<br>`--help` |  |  print syntax and exit |  |

## Using `AnnoTools`

`AnnoTools` allows you to parse a genomic annotation and convert it from/to different widely used formats. It manipulates a single in-memory genome-annotation register through a CLI-driven action stream. The action stream is built up across the command line (one switch corresponding to one action) and executed in order at the end &mdash; the same pattern used by `FASTools`. Supported formats are GFF3, GTF, and GenBank; multi-FASTA files can be read to add reference sequences to the annotation (but a GenBank file carrying an `ORIGIN` block replaces both the annotation and the reference in one pass, since its sequence is part of the record). The resulting contents of the register can be written back to any of the three formats or serialised to a compact `.Annotation` binary archive.

A typical round-trip looks like
```bash
AnnoTools --from-gff3 chr22.gff3 --to-gtf chr22.gtf
```
which loads `chr22.gff3` into a fresh register and writes it back out as a GTF file. The `--from-...` switches default to *replace* semantics &mdash; they wipe the current register and start over. The `--annotation` switch lets you instead say *add*, which extends the existing register with the new file's features:
```bash
AnnoTools --from-gff3 part1.gff3 -a add gff3 part2.gff3 --to-gff3 merged.gff3
```
Once a register has been built, options `-o` / `-i` let you cache it for later runs:
```bash
AnnoTools --from-gff3 huge.gff3 --from-fasta huge.fa -o huge        # loads huge.gff3 and writes huge.Annotation in binary format
AnnoTools -i huge --to-gtf huge.gtf                                 # reloads binary and writes contents to huge.gtf
```
Reloading is dominated by I/O (no parsing), which on GENCODE-class inputs can be orders of magnitude faster than reparsing the source.

### Parsing and validating annotation files

Annotation files are notoriously fragile, with each major format having its quirks and a number of sometimes quite divergent dialects. `AnnoTools` provides configurable parsing and validation by associating each format to a description of the same in terms of one of more *hierarchies* of features. Hierarchies can be redefined by the user, providing a general way to describe and validate content even when the annotation schema used is not entirely standard.

`AnnoTools` ships with default annotation schemas for Genbank and GTF, according to their respective standards. GFF3 input is interpreted under a built-in *standard* hierarchy by default. `AnnoTools` also provides a second built-in GFF3 dialect for the GENCODE schema, which collapses every transcript biotype into the single type `transcript` and adds the `stop_codon_redefined_as_selenocysteine` child of `CDS`; switch to it via
```bash
AnnoTools --dialect gff3 gencode --from-gff3 gencode.v47.basic.annotation.gff3 -o gencode_v47
```
The `--dialect` and `--hierarchy` overrides are *sticky*: they apply to every subsequent input operation in the named format until another `--dialect` or `--hierarchy` replaces them. To revert to a format's default, just say `--dialect <fmt> standard`. A custom hierarchy can be pinned via `--hierarchy <fmt> "<S-expression>"`.

Once loaded, the annotation (and an annotation combined with a reference sequence when the latter has been added to the register) can be validated. So
```bash
AnnoTools --from-gff3 chr22.gff3 --from-fasta chr22.fa --validate
```
loads the annotation, attaches the matching reference, and runs every consistency check implemented:
* `--validate-sequences-present` (every annotated sequence name appears in the reference)
* `--validate-feature-bounds` (every interval lies within its sequence)
* `--validate-translation` (CDS `/translation=` qualifiers agree with the translated sequence).

Switch `--validate` is the catch-all that runs all three; the individual `--validate-...` switches let you run them selectively. Each of these stops at the first violation, prints a two-line message pointing the user at `--validate-report`, and exits non-zero. For a complete enumeration, use the sibling `--validate-report <file>` action: it walks the whole register, writes one tab-separated row per violation (`check`, `path`, `feature_id`, `message`) to `<file>`, and exits non-zero only if at least one violation was found.

### The tabular format

Alongside GFF3, GTF and GenBank, `AnnoTools` reads and writes a tabular format that is the register's *text twin*: it holds everything the binary `.Annotation` archive holds, but as plain tab-separated tables you can `diff`, `grep`, `sort` and `join`. It exists because GFF3 &mdash; the obvious candidate, being already a TSV &mdash; provably cannot round-trip a register: it splits a joined feature across rows and never writes the identity that would let you rejoin them, it has no way to spell a valueless qualifier such as GenBank's `/pseudo`, and its column-9 mini-syntax has to be re-parsed before you can `cut` anything out of it.

Writing takes a *prefix* and produces three tables:
```bash
AnnoTools --from-genbank NC_000913.gb --to-tsv NC_000913
```
```
NC_000913.AnnotationFeatures.txt     id parent seq path feature_id source score strand phase intervals
NC_000913.AnnotationAttributes.txt   id key value
NC_000913.AnnotationMetadata.txt     key value
```
None of them contains a nested syntax, and every one has a fixed number of columns, so `cat`ting two together is meaningful and adding a feature with a novel attribute key does not reshape any other row. Attributes live in their own relation rather than packed into a column, which is what lets a multi-valued attribute be several rows and a valueless one be a row with an empty third field. The metadata table carries the hierarchy the annotation was read under, plus any file-level pragmas, so a register written this way can be read back without being told its schema again.

A feature's `id` is a content hash &mdash; 64-bit FNV-1a over its identity, chained through its parent &mdash; not a row number. It is therefore stable when features are inserted, and independent of row order: the `parent` column is what rebuilds the forest, so either table can be sorted and still `join`ed on the `id`. Chaining through the parent is what keeps the identical exons that alternative transcripts share distinguishable.

Reading takes the same prefix:
```bash
AnnoTools --from-tsv NC_000913 --to-genbank round-tripped.gb
```
For pipelines, a prefix under `/dev/*` &mdash; or a plain file that turns out to be one &mdash; selects a single-document form in which the three tables are introduced by `#!features`, `#!attributes` and `#!metadata` banners, so `--to-tsv /dev/stdout` composes with the rest of a shell pipeline. Note that the reference sequence is *not* part of the format, exactly as it is not part of GFF3: supply it with `--from-fasta` when you need one.

### Submitting an annotation

`--to tbl` writes an [NCBI submission feature table](https://www.ncbi.nlm.nih.gov/genbank/feature_table/), the `.tbl` that `table2asn` consumes:
```bash
AnnoTools --from-genbank NC_000913.gb --to-tbl NC_000913.tbl
```
This format is *write-only*, and deliberately so rather than by omission: it has no slot for GFF3's source column, no parent link and no annotation metadata, and `table2asn` **infers** the gene/mRNA/CDS relations from coordinate overlap and shared identifiers instead of reading them. Inference is not encoding, so nothing can be recovered from one &mdash; which is why there is no `--from-tbl`, and why the type system rather than a runtime error is what tells you so.

It is also not a substitute for the tabular format above: a `.tbl` is a stanza format wearing a TSV costume &mdash; a block header, coordinate lines, and tab-indented qualifier continuation lines &mdash; so a feature's `locus_tag` sits several lines away from its coordinates and `awk` cannot operate on it per record. The two formats exist for different jobs.

One limitation worth stating plainly: a *filtered* tabular table does not read back. `Annotation.add` requires a feature's parent to be present, so `grep CDS` over the features table produces a file whose parents are missing and which is refused. Sorting is fine; filtering is not.

### Selecting features and extracting their sequences

A *selection register* sits beside the annotation register and restricts `--selection-print` and the `--extract-*` actions to the features it matches &mdash; the same pattern, and the same switch names, that [`KPopCountDB`](https://github.com/PaoloRibeca/KPop/) uses to select spectra. A selection is expressed as a comma-separated list of `<field>~<regexp>` criteria, all of which must match:
```bash
AnnoTools --from-genbank NC_000913.gb -R 'type~CDS' --selection-print
```
A field is one of `seq`, `path`, `type`, `source`, `strand`, `id` or `label`; anything else is read as an attribute name, so `-R 'gene~dnaA'` selects on the `/gene` qualifier. An empty field name matches the feature's own id. Note that those names are reserved: an attribute that happens to share one of them cannot be selected on. The selection is sticky, starts out matching everything, and can be inverted with `--selection-negate` or reset with `--selection-clear`.

Given a reference, the selected features' sequences can then be written out as FASTA:
```bash
AnnoTools --from-genbank NC_000913.gb -R 'type~CDS' --extract-protein proteins.fasta
```
A feature's intervals are spliced in the order they are stored, so a `join(...)` location comes out as one record rather than one per exon; the result is reverse-complemented as a whole when the feature is on the minus strand. For `--extract-protein` the phase bases are dropped from the 5' end and the feature's `/transl_table` is honoured when it carries one. Because a GenBank file supplies its own sequence through its `ORIGIN` block, the example above needs nothing else; for GFF3 or GTF input, add `--from-fasta` first.

## Using `TREx`

`TREx` finds exact tandem repeats in all the sequences present in a FASTA file read on standard input, and writes a tab-separated record per identified locus to standard output (sequence name, start and end coordinates, unit length, repeat count, and the repeat unit itself). The output is convenient for downstream filtering by length, intersection against an annotation, or visualisation as a per-sequence track.

The input alphabet is configured by `--linter` (default `dna`); the algorithm-tuning options `--minimum_locus_length` and `--maximum_repeat_length` bound the search. A typical invocation requiring loci of at least 50 bp and unit length up to 10 nt looks like

```bash
TREx -m 50 -M 10 < genome.fasta > repeats.tsv
```

### Command line options for `TREx`

```
This is TREx version 4 [16-Apr-2024]
 compiled against: BiOCamLib version 500 [06-Apr-2026]
 (c) 2023-2024 Paolo Ribeca <paolo.ribeca@gmail.com>
```
*Usage:*
```
TREx [OPTIONS]
```

**Input/Output**

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `-l`<br>`--linter` | _'none'&#124;'DNA'&#124;'dna'&#124;'protein'_ |  sets linter for sequence\.<br>All non\-base \(for DNA\) or non\-AA \(for protein\) characters  are converted to unknowns | <ins>default=<mark>_dna_</mark></ins> |
| `--linter-keep-lowercase` | _&lt;bool&gt;_ |  sets whether the linter should keep lowercase DNA/protein characters  appearing in sequences rather than capitalise them | <ins>default=<mark>_false_</mark></ins> |
| `--linter-keep-dashes` | _&lt;bool&gt;_ |  sets whether the linter should keep dashes appearing in sequences  rather than convert them to unknowns | <ins>default=<mark>_false_</mark></ins> |

**Algorithm**

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `-M`<br>`--maximum_repeat_length` | _&lt;non\_negative\_integer&gt;_ |  maximum unit length for a tandem repeat to be considered | <ins>default=<mark>_4611686018427387903_</mark></ins> |
| `-m`<br>`--minimum_locus_length` | _&lt;non\_negative\_integer&gt;_ |  minimum locus length for a tandem repeat to be considered | <ins>default=<mark>_0_</mark></ins> |

**Miscellaneous**

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `-v`<br>`--verbose` |  |  set verbose execution \(global option\) | <ins>default=<mark>_quiet execution_</mark></ins> |
| `-V`<br>`--version` |  |  print version and exit |  |
| `-h`<br>`--help` |  |  print syntax and exit |  |

## Using `Cophenetic`

`Cophenetic` reads a Newick tree on standard input and writes the cophenetic-distance matrix between every pair of labelled nodes (leaves and internal alike) to standard output, as a tab-separated table with row and column headers. By default the distances are shortest-path along branch lengths &mdash; the standard cophenetic interpretation. The `-M`/`--max-distances` switch produces the longest-path matrix instead, which is the right convention when branch lengths encode partial quantities such as allele frequencies (so two siblings of a common ancestor with branch lengths 0.3 and 0.5 sit at "distance" 0.5 rather than 0.8). The matrix computation is parallelised across the cores chosen by `-t`/`--threads`.

```bash
Cophenetic -t 4 < tree.nwk > distances.tsv
```

### Command line options for `Cophenetic`

```
This is Cophenetic version 2 [06-Jun-2024]
 compiled against: BiOCamLib version 500 [06-Apr-2026]
 (c) 2024 Paolo Ribeca <paolo.ribeca@gmail.com>
```
*Usage:*
```
Cophenetic [OPTIONS]
```

**Algorithm**

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `-M`<br>`--max-distances`<br>`--maximum-distances`<br>`--longest-path-distances` |  |  compute longest\- rather than shortest\-path distances | <ins>default=<mark>_compute shortest\-path distances_</mark></ins> |

**Miscellaneous**

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `-t`<br>`-T`<br>`--threads` | _&lt;computing\_threads&gt;_ |  number of concurrent computing threads to be spawned  \(default automatically detected from your configuration\) | <ins>default=<mark>_4_</mark></ins> |
| `-v`<br>`--verbose` |  |  set verbose execution \(global option\) | <ins>default=<mark>_false_</mark></ins> |
| `-V`<br>`--version` |  |  print version and exit |  |
| `-h`<br>`--help` |  |  print syntax and exit |  |

## Using `Yggdrasill`

`Yggdrasill` builds an unrooted phylogenetic tree from a register of weighted splits. The natural input is a `.PhyloSplits` file produced by [`KPopTwistDB`](https://github.com/PaoloRibeca/KPop/)'s `--splits` action, but any tool that emits the same format can drive it. The action-language semantics mirrors `KPopCountDB` and `KPopTwistDB`: each command-line flag enqueues one register operation, the operations are executed in command-line order, and combinations of `-i`/`-I`/`-a`/`-A`/`-t`/`-o`/`-O` let you load, merge, build trees from, and dump splits in any order.

The central operation is `-t`/`--tree <prefix>`: it reads the live splits register, greedy-filters the splits in order of decreasing weight for pairwise compatibility, assembles the accepted subset into a Newick tree via the Buneman construction, and writes the tree to `<prefix>.nwk` and the compatible-splits subset to `<prefix>.PhyloSplits`. The incompatible residual is left in the register, so a subsequent `-t` pass can build a "tree of residuals", or the residual can be written out via `-o`/`-O` for inspection.

After every `-t` pass, a one-line summary is emitted to stderr reporting how many compatible splits were accepted, how many were dropped as incompatible, the total weight on each side, and the ratio of dropped to total weight. That ratio is a direct measure of how tree-like the input split system was: values near zero indicate the data is essentially tree-like, while values approaching one signal genuine network-like structure that the Buneman construction necessarily projects away.

A typical end-to-end pipeline taking a `KPopTwistDB` splits output and producing both the tree and the residual is

```bash
KPopTwistDB -i T train -i t train --splits-algorithm gaps -S splits
Yggdrasill  -i splits -t tree -O dropped
```

### Command line options for `Yggdrasill`

```
This is Yggdrasill version 5 [05-Feb-2025]
 compiled against: BiOCamLib version 500 [06-Apr-2026]
 (c) 2024-2025 Paolo Ribeca <paolo.ribeca@gmail.com>
```
*Usage:*
```
Yggdrasill [OPTIONS]
```

**Actions\.**
They are executed delayed and in order of specification\.

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `-c`<br>`--clear` |  |  clear the splits register \(keep names, discard existing splits\) |  |
| `-i`<br>`--input` | _&lt;binary\_file\_prefix&gt;_ |  load into the splits register the specified binary database  \(which must have extension '\.PhyloSplits' unless file is '/dev/\*'\) |  |
| `-I`<br>`--Input` | _&lt;splits\_file\_prefix&gt;_ |  load into the splits register the specified plain text database  \(which must have extension '\.PhyloSplits\.txt' unless file is '/dev/\*'\) |  |
| `-a`<br>`--add` | _&lt;binary\_file\_prefix&gt;_ |  add to the contents of the splits register the specified binary database  \(which must have extension '\.PhyloSplits' unless file is '/dev/\*'\) |  |
| `-A`<br>`--Add` | _&lt;splits\_file\_prefix&gt;_ |  add to the contents of the splits register the specified plain text database  \(which must have extension '\.PhyloSplits\.txt' unless file is '/dev/\*'\) |  |
| `-t`<br>`--tree` | _&lt;tree\_file\_prefix&gt;_ |  generate a phylogenetic tree from the contents of the splits register\.<br>The results will be a Newick file  \(which will be given extension '\.nwk' unless file is '/dev/\*'\) and the database of compatible splits used to build the tree  \(which will be given extension '\.PhyloSplits\.txt' unless file is '/dev/\*'\)\.<br>The residual incompatible splits will be moved back to the splits register |  |
| `-o`<br>`--output` | _&lt;binary\_file\_prefix&gt;_ |  dump the contents of the splits register to the specified binary file  \(which will be given extension '\.PhyloSplits' unless file is '/dev/\*'\) |  |
| `--precision` | _&lt;positive\_integer&gt;_ |  set the number of precision digits to be used when outputting numbers | <ins>default=<mark>_10_</mark></ins> |
| `-O`<br>`--Output` | _&lt;splits\_file\_prefix&gt;_ |  dump the contents of the splits register to the specified plain text file  \(which will be given extension '\.PhyloSplits\.txt' unless file is '/dev/\*'\) |  |

**Miscellaneous options\.**
They are set immediately\.

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `-v`<br>`--verbose` |  |  set verbose execution \(global option\) | <ins>default=<mark>_quiet execution_</mark></ins> |
| `-V`<br>`--version` |  |  print version and exit |  |
| `-h`<br>`--help` |  |  print syntax and exit |  |

### Command line options for `AnnoTools`

This is the full list of command line options available for the program `AnnoTools`. You can visualise the list by typing
```bash
AnnoTools -h
```
in your terminal. You will see a header containing information about the version:
```
This is AnnoTools version 1 [09-May-2026]
 compiled against: BiOCamLib version 506 [09-May-2026]
 (c) 2026 Paolo Ribeca <paolo.ribeca@gmail.com>
```
followed by detailed information. The general form(s) the command can be used is:
```
AnnoTools [ACTIONS]
```

**Actions.**
They are executed delayed and in order of specification.

*Operations on the annotation register:*

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `-0`<br>`--empty` |  |  load an empty annotation into the register |  |
| `-i`<br>`--input` | _binary\_file\_prefix_ |  load into the register the annotation present in the specified binary file \(extension '\.Annotation' is appended unless the path is under '/dev/\*'\) |  |
| `-o`<br>`--output` | _binary\_file\_prefix_ |  write the current register to the specified binary file \(extension '\.Annotation' is appended unless under '/dev/\*'\) |  |

*Hierarchy.*
Override the active hierarchy for a given format. The override is sticky: every subsequent input operation in that format uses it until another `--hierarchy` or `--dialect` replaces it. Reverting to the format's default is just `--dialect <fmt> standard`.

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `--hierarchy` | `gff3`&#124;`gtf`&#124;`genbank` _S\-expression_ |  set the hierarchy to use for subsequent input operations in the named format |  |
| `--dialect` | `gff3`&#124;`gtf`&#124;`genbank` _name_ |  switch subsequent input operations in the named format to one of its built\-in dialects\.<br>Currently only GFF3 ships more than one dialect \(`standard` and `gencode`\) |  |

*Annotation input.*
Long form: action mode + format + path. Short forms `--from-gff3`, `--from-gtf`, `--from-genbank` default to `replace`.

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `-a`<br>`--annotation` | `replace`&#124;`add` `gff3`&#124;`gtf`&#124;`genbank` _file_ |  merge or replace the register from _file_ in the named format\.<br>When the format is GenBank and the input carries an ORIGIN section, the reference sequence is replaced as well |  |
| `--from-gff3` | _file_ |  shorthand for `--annotation replace gff3 <file>` |  |
| `--from-gtf` | _file_ |  shorthand for `--annotation replace gtf <file>` |  |
| `--from-genbank` | _file_ |  shorthand for `--annotation replace genbank <file>` |  |
| `--from-tsv`<br>`--from-tabular` | _file\_or\_prefix_ |  shorthand for `--annotation replace tsv <file_or_prefix>`; reads the three `.Annotation*.txt` tables written from the given prefix |  |

*Reference (multi-FASTA) input.*
Long form takes the same mode keyword as `--annotation`. Short form `--from-fasta` defaults to `replace`.

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `-r`<br>`--reference` | `replace`&#124;`add` _file_ |  merge or replace the register's reference from _file_ |  |
| `--from-fasta` | _file_ |  shorthand for `--reference replace <file>` |  |

*Validation.*
Each check raises and exits non-zero on the first violation. All require a reference to be set.

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `--validate-sequences-present` |  |  every sequence referenced by an annotation feature must also exist in the reference |  |
| `--validate-feature-bounds` |  |  every feature interval must lie within the corresponding sequence's length |  |
| `--validate-translation` |  |  translated CDS features must agree with their `/translation=` qualifier |  |
| `--validate` |  |  run every validation in turn |  |
| `--validate-report` | _file_ |  run every validation against the whole register \(do not stop at the first violation\) and write a tab\-separated report \(`check`, `path`, `feature_id`, `message`\) to _file_; exit non\-zero iff any violation was found |  |
| `--summary` |  |  print a one\-line summary of the current register to stderr |  |

*Actions involving the selection register.*
The selection restricts `--selection-print` and the `--extract-*` actions to the features it matches. The `--to-*` writers and `-o` always write the whole register, because a feature whose parent is not selected would be emitted without it. The selection is sticky, and starts out matching everything.

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `-L`<br>`--labels`<br>`--selection-from-labels` | _feature\_id_\[`,`...`,`_feature\_id_\] |  put into the selection register the features carrying the specified ids |  |
| `-R`<br>`--regexps`<br>`--selection-from-regexps` | _field_`~`_regexp_\[`,`...`,`_field_`~`_regexp_\] |  put into the selection register the features whose fields match the specified regexps. All criteria must match. A field is one of `seq`, `path`, `type`, `source`, `strand` or `id`, or else the name of an attribute, in which case the criterion matches when any one of that attribute's values does. An empty field name makes the regexp match feature ids |  |
| `--selection-negate` |  |  negate the current selection |  |
| `--selection-print` |  |  print the features currently selected, one per line, to standard output |  |
| `--selection-clear` |  |  reset the selection register so that it matches everything |  |

*Sequence extraction.*
Emit the sequence denoted by each selected feature as FASTA. A feature's intervals are spliced in the order they are stored and the result is reverse-complemented when the feature is on the minus strand; a protein is that sequence with the phase bases dropped from its 5' end, translated with the feature's `/transl_table` when it carries one. Requires a reference to have been loaded.

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `--extract` | `dna`&#124;`protein` _file_ |  write the sequence of every selected feature to _file_ |  |
| `--extract-dna` | _file_ |  shorthand for `--extract dna <file>` |  |
| `--extract-protein` | _file_ |  shorthand for `--extract protein <file>` |  |

*Annotation output.*

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `--to` | `gff3`&#124;`gtf`&#124;`genbank`&#124;`tsv`&#124;`tbl` _file\_or\_prefix_ |  write the register to _file\_or\_prefix_ in the named format. `tbl` is NCBI's submission feature table, which is write\-only |  |
| `--to-gff3` | _file_ |  shorthand for `--to gff3 <file>` |  |
| `--to-gtf` | _file_ |  shorthand for `--to gtf <file>` |  |
| `--to-genbank` | _file_ |  shorthand for `--to genbank <file>` |  |
| `--to-tsv`<br>`--to-tabular` | _file\_or\_prefix_ |  shorthand for `--to tsv <file_or_prefix>`; writes the three `.Annotation*.txt` tables from the given prefix |  |
| `--to-tbl`<br>`--to-feature-table` | _file_ |  shorthand for `--to tbl <file>`; writes an NCBI submission feature table |  |

**Miscellaneous options.**
They are set immediately.

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `-v`<br>`--verbose` |  |  set verbose execution | <ins>default=<mark>_quiet execution_</mark></ins> |
| `-V`<br>`--version` |  |  print version and exit |  |
| `-h`<br>`--help` |  |  print syntax and exit |  |

