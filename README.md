# simchk

Simchk (similarity checker) is a small program for finding similar lines within
a file, useful for refactoring purposes.  It does this using the Levenshtein/edit
distance of a string.

## Build

Only requires ocaml and make

```sh
$ make
```

## Usage

```
usage: simchk <filename> [lower-boundary] [upper-boundary] [exclude-regex]
```

Simchk takes the filename to read from and optionally 1-3 extra parameters
to filter the output.  The upper and lower boundaries are in the range
0-100 and will filter out line pairs that are outside of a (lower..upper)% match.
These default to 20 and 100 respectively.  Exclude-regex can also optionally
be specfied to exclude any lines of output with any subsection matching the given regex.
