
<!-- README.md is generated from README.Rmd. Please edit that file -->

# macrtools

<!-- badges: start -->

[![R-CMD-check](https://github.com/rmacoslib/macrtools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rmacoslib/macrtools/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**This is a work in progress package. Features may be unstable.**

**This is an *unofficial* project to support users on macOS.**

The repository contains `macrtools` a macOS-only *R* package. The
package is setup to re-create the compiled code toolchain for the [CRAN
official macOS *R* binary](https://cran.r-project.org/bin/macosx/) that
handles setting up the macOS R development toolchain as described on the
[macOS `tools` r-project developer
page](https://mac.r-project.org/tools/).

**Note:** The installer package was developed by [James Joseph
Balamuta](https://thecoatlessprofessor.com/) and has no connection with
the R project’s macOS CRAN maintainers.

## Quick Start

For an overview of how to use the package, please see the following
video:

[![Video: Installing and using the `{macrtools}` package to setup the R
Compilation Toolchain for
macOS](http://img.youtube.com/vi/_fckF0fefXQ/0.jpg)](https://www.youtube.com/watch?v=_fckF0fefXQ)

### Install the R Package

You can install the development version of `macrtools` from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("rmacoslib/macrtools")
```

### Install the R Compilation Toolchain using the R Package

With the package installed, the required components can be installed
using:

``` r
macrtools::macos_rtools_install()
```

This will attempt to install:

- Xcode CLI
- gfortran
- R Development binaries

The compilation toolchain can be removed by using:

``` r
macrtools::macos_rtools_uninstall()
```

## Usage

The package is designed to help diagnose, install, and uninstall
different parts of the macOS R compilation toolchain.

### Diagnose

The package features two tools that seek to identify if core components
are installed:

``` r
# We can check if Xcode CLI is present using:
macrtools::is_xcode_cli_installed()

# We can verify gfortran is present as well
macrtools::is_gfortran_installed()
```

### Installation

The next set of functions focus primarily on installing different binary
packages into the required locations.

``` r
# We can perform a non-interactive installation of Xcode CLI with:
macrtools::xcode_cli_install() 

# We can install gfortran using:
macrtools::gfortran_install()

# And other binaries required for compiling R using:
macrtools::recipes_binary_install('r-base-dev')
```

### Uninstall

If we no longer have interest in having development tools present, we
can remove the development tools using:

``` r
# We can remove Xcode CLI using
macrtools::xcode_cli_uninstall()

# We can uninstall gfortran using:
macrtools::gfortran_uninstall()
```

**Note:** This does not yet uninstall the *R* development binaries.

## License

AGPL (\>=3)
