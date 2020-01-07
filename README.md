# till

> Watch a command's output until the output matches some conditions.

### Motivation

Learning. :nerd_face:

### Usage

```
till v0.0.1

Execute a command until it's output matches certain conditions.

Project's Home Page: https://github.com/gilchristian/till

Usage: till CMD [PATTERN] [-i|--interval SECONDS] [-e|--continue-on-error]

Available options:
  CMD                      Command to run
  PATTERN                  Patterns to match against CMD's output
  -i,--interval SECONDS    The interval (in seconds) to run CMD
  -e,--continue-on-error   Keep trying if CMD exits with non zero result
  -h,--help                Show this help text
```

### Install

Only [stack](https://www.haskellstack.org/) installation supported (for now :tm:):

```
git clone git@github.com:gillchristian/till.git
cd till
stack install .
```

### TODO

- [x] Print output on full screen.
- [x] Cleanup last (or first?) output (it's left after finishing, but with cleared screen, ie. can be seen when scrolling). Other option is to show last output (without scroll) and highlight match.
- [ ] Send desktop notification on completion (opt-in). See [fdo-notify](https://hackage.haskell.org/package/fdo-notify). Optionally [play a beep sound](http://hackage.haskell.org/package/honk-1.3.0.0/docs/Sound-Honk.html) instead (also opt-in).
- [ ] Keep track of time and/or attempts it took to finish.
- [ ] Watch "forever" if no patterns are provided.
- [ ] Highlight matches (opt-in).
- [ ] Support multiple patterns (eg. two lines that meet same requirement, instead of only one, or two lines that match different things).
