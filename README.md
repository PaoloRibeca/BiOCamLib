# BiOCamLib

BiOCamLib is the [OCaml](https://ocaml.org) foundation upon which a number of the bioinformatics tools I developed are built.

It mostly consists of a library &mdash; you'll need to clone this repository if you want to manually compile other programs I've developed, notably [SiNPle](https://github.com/PaoloRibeca/SiNPle) or [KPop](https://github.com/PaoloRibeca/KPop). You might also use the library for your own programs, if you are familiar with OCaml and patient enough to read the code.

As a bonus, BiOCamLib comes bundled with a few programs:
* `RC`, which can efficiently compute the reverse complement of (possibly very long) sequences. Each sequence should be input on a separate line &mdash; lines are processed one by one and not buffered. I use this program in many of my workflows.
* `Octopus`, which is a high-throughput program to compute the transitive closure of strings. This is useful to cluster things.
* `Parallel`, which allows you to split and process an input file chunk-wise using the reader/workers/writer model implemented in `BiOCamLib.Tools.Parallel`. You can see it as a demonstration of the capabilities of the library, but I also often use it as a useful tool to solve real-life problems.
* `FASTools`, which is a Swiss-knife tool for the manipulation of FASTA/FASTQ files. It supports all formats (FASTA, single- and paired-end FASTQ, interleaved FASTQ) and a simpler tabular format whereby FASTA/FASTQ records are represented as tab-separated lines. It facilitates format interconversions and other manipulations.

## Installing `RC`, `Octopus`, `Parallel`, and `FASTools`

> :warning: Note that the only operating systems we officially support are Linux and MacOS. :warning:
>
> OCaml is highly portable and you might be able to manually compile/install everything successfully on other platforms (for instance, Windows) but you will have to do it yourself. 

There are several possible ways of installing the software on your machine: through `conda`; by downloading pre-compiled binaries (Linux and MacOS x86_64 only); or manually.

### Conda channel

> :construction: Coming soon! :construction:

### Pre-compiled binaries

You can download pre-compiled binaries for Linux and MacOS x86_64 from our [releases](https://github.com/PaoloRibeca/BiOCamLib/releases).

### Manual install

Alternatively, you can install `RC`, `Octopus`, `Parallel`, and `FASTools` manually by cloning and compiling the BiOCamLib sources. You'll need an up-to-date distribution of the OCaml compiler and the [Dune package manager](https://github.com/ocaml/dune) for that. Both can be installed through [OPAM](https://opam.ocaml.org/), the official OCaml distribution system. Once you have a working OPAM distribution you'll also have a working OCaml compiler, and Dune can be installed with the command
```bash
$ opam install dune
```
if it is not already present. Make sure that you install OCaml version 4.12 or later.

Then go to the directory into which you have downloaded the latest BiOCamLib sources, and type
```bash
$ ./BUILD
```

That should generate the executables `RC`, `Octopus`, `Parallel`, and `FASTools`. Copy them to some favourite location in your PATH, for instance `~/.local/bin`.

## Using `RC`

`RC` inputs sequences from standard input and outputs their reverse complement to standard output, one sequence at the time. Hence, `RC` can be conveniently used in a subprocess. For example, the command
```bash
$ echo GAtTaCA | RC
```
would produce `TGtAaTC`. Note that non-`[ACGTacgt]` characters are output unmodified, so sequence validation and linting must be performed elsewhere whenever they are necessary.

### Command line options

This is the full list of command line options available for the program `RC`. You can visualise the list by typing
```bash
$ RC -h
```
in your terminal. You will see a header containing information about the version:
```
This is the RC program (version 0.2)
 (c) 2023 Paolo Ribeca, <paolo.ribeca@gmail.com>
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
$ (cat <<___
A duh
  b  C
c f e
 duh zz x
b c
___
) | Octopus
```
(without the first `$` prompt character) will result in the output
```
A       duh     x       zz
C       b       c       e       f
```
(tab-separated).

### Command line options for `Octopus`

This is the full list of command line options available for the program `Octopus`. You can visualise the list by typing
```bash
$ Octopus -h
```
in your terminal. You will see a header containing information about the version:

```
This is the Octopus program (version 0.4)
 (c) 2016-2023 Paolo Ribeca, <paolo.ribeca@gmail.com>
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

## Command line options for `Parallel`

This is the full list of command line options available for the program `Parallel`. You can visualise the list by typing
```bash
$ Parallel -h
```
in your terminal. You will see a header containing information about the version:
```
This is the Parallel program (version 0.4)
 (c) 2019-2022 Paolo Ribeca, <paolo.ribeca@gmail.com>
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
| `-l`<br>`--lines-per-block` | _&lt;positive\_integer&gt;_ |  number of lines to be processed per block | <ins>default=<mark>_10000_</mark></ins> |
| `-i`<br>`--input` | _&lt;input\_file&gt;_ |  name of input file | <ins>default=<mark>_stdin_</mark></ins> |
| `-o`<br>`--output` | _&lt;output\_file&gt;_ |  name of output file | <ins>default=<mark>_stdout_</mark></ins> |

**Miscellaneous**

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `-t`<br>`--threads` | _&lt;positive\_integer&gt;_ |  number of concurrent computing threads to be spawned  \(default automatically detected from your configuration\) | <ins>default=<mark>_nproc_</mark></ins> |
| `-v`<br>`--verbose` |  |  set verbose execution | <ins>default=<mark>_false_</mark></ins> |
| `-d`<br>`--debug` |  |  output debugging information | <ins>default=<mark>_false_</mark></ins> |
| `-h`<br>`--help` |  |  print syntax and exit |  |

## Command line options for `FASTools`

This is the full list of command line options available for the program `FASTools`. You can visualise the list by typing
```bash
$ FASTools -h
```
in your terminal. You will see a header containing information about the version:
```
This is the FASTools program (version 0.5)
 (c) 2022-2023 Paolo Ribeca, <paolo.ribeca@gmail.com>
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
| `match`<br>`-m`<br>`--match` | _&lt;regexp&gt;_ |  select matching sequence names in FASTA/FASTQ records or tab\-separated lines\.<br>For paired\-end files, the pair matches when at least one name matches |  |
| `revcom`<br>`-r`<br>`--revcom` |  |  reverse\-complement sequences in FASTA/FASTQ records or tab\-separated lines |  |
| `dropq`<br>`-d`<br>`--dropq` |  |  drop qualities in FASTA/FASTQ records or tab\-separated lines |  |

**Input/Output.**
Executed delayed in order of specification, default=<mark>_-F_</mark>.

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `-f`<br>`--fasta` | _&lt;fasta\_file\_name&gt;_ |  process FASTA input file containing sequences |  |
| `-F` |  |  process FASTA sequences from standard input |  |
| `-s`<br>`--single-end` | _&lt;fastq\_file\_name&gt;_ |  process FASTQ input file containing single\-end sequencing reads |  |
| `-S` |  |  process single\-end FASTQ sequencing reads from standard input |  |
| `-p`<br>`--paired-end` | _&lt;fastq\_file\_name1&gt; &lt;fastq\_file\_name2&gt;_ |  process FASTQ input files containing paired\-end sequencing reads |  |
| `-P` |  |  process interleaved FASTQ sequencing reads from standard input |  |
| `-t`<br>`--tabular` | _&lt;tabular\_file\_name&gt;_ |  process input file containing FAST\[A&#124;Q\] records as tab\-separated lines |  |
| `-T` |  |  process FAST\[A&#124;Q\] records in tabular form from standard input |  |
| `-l`<br>`--linter` | _'none'&#124;'DNA'&#124;'dna'&#124;'protein'_ |  sets linter for sequence\.<br>All non\-base \(for DNA\) or non\-AA \(for protein\) characters  are converted to unknowns | <ins>default=<mark>_none_</mark></ins> |
| `--linter-keep-lowercase` | _&lt;bool&gt;_ |  sets whether the linter should keep lowercase DNA/protein characters  appearing in sequences rather than capitalise them | <ins>default=<mark>_false_</mark></ins> |
| `--linter-keep-dashes` | _&lt;bool&gt;_ |  sets whether the linter should keep dashes appearing in sequences  or convert them to unknowns | <ins>default=<mark>_false_</mark></ins> |
| `-o`<br>`--output` | _&lt;output\_file\_name&gt;_ |  set the name of the output file\.<br>Files are kept open, and it is possible to switch between them  by repeatedly using this option\.<br>Use '/dev/stdout' for standard output | <ins>default=<mark>_/dev/stdout_</mark></ins> |
| `-O`<br>`--paired-end-output` | _&lt;output\_file\_name\_1&gt; &lt;output\_file\_name\_2&gt;_ |  set the names of paired\-end FASTQ output files\.<br>Files are kept open, and it is possible to switch between them  by repeatedly using this option\.<br>Use '/dev/stdout' for standard output | <ins>default=<mark>_/dev/stdout /dev/stdout_</mark></ins> |
| `--flush`<br>`--flush-output` |  |  flush output after each record \(global option\) | <ins>default=<mark>_do not flush_</mark></ins> |

**Miscellaneous**

| Option | Argument(s) | Effect | Note(s) |
|-|-|-|-|
| `-v`<br>`--verbose` |  |  set verbose execution \(global option\) | <ins>default=<mark>_false_</mark></ins> |
| `-h`<br>`--help` |  |  print syntax and exit |  |

