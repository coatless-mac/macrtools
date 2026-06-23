# Execute shell command with sudo

Execute shell command with sudo

## Usage

``` r
shell_sudo_command(cmd, password, verbose = TRUE, prefix = "sudo -kS ")
```

## Arguments

- cmd:

  The command to execute

- password:

  User password for sudo privileges

- verbose:

  Display the command being executed

- prefix:

  The sudo prefix (default: "sudo -kS ")

## Value

The exit status of the command (0 for success)
