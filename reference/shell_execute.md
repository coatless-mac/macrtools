# Execute a shell command

Execute a command with or without sudo privileges

## Usage

``` r
shell_execute(
  cmd,
  sudo = FALSE,
  password = NULL,
  verbose = TRUE,
  timeout = 300
)
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

- timeout:

  Timeout in seconds (default: 300)

## Value

The exit status of the command (0 for success)
