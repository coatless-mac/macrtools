# Install and Uninstall the macOS R Toolchain

The `macos_rtools_install()` function aims to install all required
dependencies for the macOS R compilation toolchain. Meanwhile, the
`macros_rtools_uninstall()` function aims to remove any installed files
from your computer.

## Usage

``` r
macos_rtools_install(
  password = base::getOption("macrtools.password"),
  verbose = TRUE
)

macos_rtools_uninstall(
  password = base::getOption("macrtools.password"),
  verbose = TRUE
)
```

## Arguments

- password:

  Password for user account to request `sudo` access.

- verbose:

  Describe each step taken. Default `TRUE`

## Details

The macOS R compilation toolchain consists of:

1.  Xcode CLI

2.  gfortran

3.  A series of binary packages from the
    [`recipes`](https://github.com/R-macos/recipes) system to compile R.

The `mac_rtools_install()` function attempts to install each of the
required components. If we detect that the Xcode.app IDE is installed,
we'll skip attempting to install the Xcode CLI software.

Meanwhile, the `mac_rtools_uninstall()` function aims to delete or
uninstall the Xcode CLI and gfortran binaries. At the present moment,
there is no support for uninstalling the binary packages from `recipes`.

## Headless or Unattended Installation

For an installation that does not require a human in the loop, please
set the environment variable `macrtools.password` for the user profile:

    Sys.setenv("macrtools.password" = "your-password-here")

## Examples

``` r

if (FALSE) { # \dontrun{

# Install all tools required for compiling code on macOS with R
macos_rtools_install()

# Enter a password to avoid being prompted!
macos_rtools_install(password = "hello-compiled-code-world")

} # }

if (FALSE) { # \dontrun{

# Remove Xcode CLI and gfortran tools required for compiling code on macOS with R
macos_rtools_uninstall()

# Enter a password to avoid being prompted!
macos_rtools_uninstall(password = "hello-compiled-code-world")

} # }
```
