# most-used [![Build Status](https://travis-ci.org/gabebw/most-used.svg?branch=master)](https://travis-ci.org/gabebw/most-used)

This will find your most-used shell commands. It automatically detects whether
you're using Zsh or Bash and prints out the history for the correct shell.

To try it out:

```
$ brew tap gabebw/formulae
$ brew install most-used
$ most-used | head
```

## Local development

After changing something, you can run this to recompile and see the result of
parsing history:

    ./bin/run-zsh

Or, to parse your Bash history:

    ./bin/run-bash

You can also pass options:

    ./bin/run-zsh --include-first-argument=git --include-first-argument=spring

To print lines that failed to parse:

    ./bin/run-zsh --debug

To run tests:

    ./bin/test
