## v0.0.5 (August 1, 2017)

* Parse lines with shell variables (like `echo $var`)
* Commands in the same history item separated by a pipe are counted as separate
  commands
* Parse commands that end in an escaped newline
* Parse commands that have $-quoted arguments like `echo $'hello'`
* Add `--debug` switch to print only lines that failed to parse

## v0.0.4 (July 27, 2017)

* Use specific dependency versions for Cabal, so that Homebrew installation is
  more predictable

## v0.0.3 (July 27, 2017)

* Ship with a shell script named `most-used` that pipes `fc` output for the user

## v0.0.2 (July 27, 2017)

* Rename the Haskell executable to most-used

## v0.0.1 (July 26, 2017)

* First version
