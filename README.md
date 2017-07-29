# most-used

This takes input from Zsh's `fc` command and parses it to find your most-used
commands.

To try it out:

```
$ brew tap gabebw/formulae
$ brew install most-used
$ most-used | head
```

## Local development

After changing something, you can run this to recompile and see the result of
parsing history:

    ./bin/run

You can also pass options:

    ./bin/run --include-first-argument=git --include-first-argument=spring
