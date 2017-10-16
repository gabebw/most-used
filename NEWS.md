## v0.0.7.1 (October 15 2017)

* Fix version string so `--version` returns the correct version

## v0.0.7 (October 15 2017)

* The program learned to use semicolons as command delimiters, so that
  `foo; bar` is understood as two commands (#34, #36)

## v0.0.6.1 (September 2, 2017)

* Fix version string so `--version` returns the correct version

## v0.0.6 (August 21, 2017)

* Parse Bash history, not just Zsh history
* Add a `--version` flag
* Parse nested process substitution, like `$(echo $(echo hello))`
* Parse heredocs, like `cat <<<hello`

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
