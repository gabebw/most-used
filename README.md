# most-used

This can parse the contents of `~/.zsh_history`.

It assumes you have `setopt extended_history`, so that your `~/.zsh_history`
looks like this:

```
: 1407468397:0;git shortlog -s |\
 cut -f1 |\
 spark
: 1407468436:0;g show 3435d20
```

## Future Goals

* [ ] Print out which commands are used the most
* [ ] Include the first argument as part of some commands (e.g. "spring rspec")
* [ ] Parse the `~/.zsh_history` file directly, instead of taking in strings
