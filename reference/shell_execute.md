# Execute a shell command

Execute a command with or without sudo privileges

## Usage

``` r
shell_execute(cmd, sudo = FALSE, password = NULL, verbose = TRUE)
```

## Arguments

- cmd:

  The command to execute

- sudo:

  Whether to use sudo (default: FALSE)

- password:

  User password for sudo (only required when sudo=TRUE)

- verbose:

  Display the command being executed

## Value

The exit status of the command (0 for success)
