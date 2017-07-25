# most-used

This takes input from Zsh's `fc` command and parses it to find your most-used
commands.

To try it out:

```
./bin/run
```

`fc` output looks like this:

```
  474  g push origin master
  475  open file
  476  echo hello
  477  fc -l | tail -4
```

Usage (eventually):

```
$ fc -l 1 | most-used
```

## Future Goals

* [ ] Print out which commands are used the most
* [ ] Include the first argument as part of some commands (e.g. "spring rspec")
* [ ] Parse the `~/.zsh_history` file directly, instead of taking in strings
