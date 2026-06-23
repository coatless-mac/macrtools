# Capture the Text Output of an External Command

Runs `command` with `args` via
[`sys::exec_internal()`](https://jeroen.r-universe.dev/sys/reference/exec.html)
and returns its standard output as trimmed text. If the command cannot
be run, `fallback` is returned instead.

## Usage

``` r
exec_text(command, args, fallback = "Unknown")
```

## Arguments

- command:

  Name of the program to execute.

- args:

  Character vector of arguments passed to the program.

- fallback:

  Value returned when execution fails. Default `"Unknown"`.

## Value

The command's standard output as text, or `fallback` on error.
