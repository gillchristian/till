# till

> Watch a command's output until the output matches some conditions.

### Usage

```
$ till --help
till v0.0.1
Execute a command until its output matches certain conditions.
Project's Home Page: https://github.com/gilchristian/till

Usage: till [OPTIONS] <CMD> [PATTERNS]...

Arguments:
  <CMD>
          Command to run

  [PATTERNS]...
          Patterns (mini expr: quotes allowed; ! negates; & and | combine)

Options:
      --interval <INTERVAL>
          Interval (seconds) to run CMD

          [default: 2]

      --continue-on-error
          Keep trying if CMD exits with non-zero status

  -h, --help
          Print help (see a summary with '-h')

  -V, --version
          Print version
```

### Install

Build from source with Rust (*I use Rust btw*).

```
git clone git@github.com:gillchristian/till.git
cd till
cargo install --path .
```
