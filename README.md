
<!-- README.md is generated from README.Rmd. Please edit that file -->

# macrtools

<!-- badges: start -->

[![R-CMD-check](https://github.com/coatless-mac/macrtools/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rmacoslib/macrtools/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**This is a work in progress package. Features may be unstable.**

**This is an *unofficial* project to support users on macOS.**

The repository includes source code for `macrtools`, which is an *R*
package exclusively for [Apple](https://www.apple.com/)’s macOS
operating system. This package is designed to recreate the compiled code
toolchain used to compile the [official macOS R binary on
CRAN](https://cran.r-project.org/bin/macosx/) by following the steps
described on the [r-project developer page for macOS
`tools`](https://mac.r-project.org/tools/). The package is able to to
setup the compilation toolchain on any Mac that meets the standards
required to install the official CRAN *R* binary on macOS.

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
remotes::install_github("coatless-mac/macrtools")
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
- R Development binaries from the Recipes project

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

The package can also check to see if the full version of the Xcode
(Xcode.app IDE) is installed.

``` r
# See if Xcode.app IDE is in use
macrtools::is_xcode_app_installed()
```

The Xcode.app is a significantly larger development toolkit compared to
Xcode CLI.

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

## Design Decisions

> Why not a macOS installer package (`.pkg`) or Apple Disk Image
> (`.dmg`) ?

We previously wrote two very successful installer packages (
[`r-macos-rtools`](https://github.com/coatless-mac/r-macos-rtools) and
[`r-macos-clang`](https://github.com/coatless-mac/r-macos-clang)).
However, as part of the up-keep of the installer package, we needed to
have a yearly subscription to Apple’s [Developer
program](https://developer.apple.com/), which has a base cost of ~\$99
and sales tax of about ~\$6 leading to an annual expense of ~\$105 per
year. Given that we only used the Developer account to sign and notarize
only the installers packages, we opted not to continue on this path.

That said, we are grateful to [Professor Timothy
Bates](http://www.ed.ac.uk/profile/timothy-bates) of the [University of
Edinburgh](http://www.ed.ac.uk/). He provided the initial financial
support that allowed for the installer to be signed and tested.

Moreover, we hope that by placing the logic inside of an *R* package,
the community of *R* developers will be able to play a more active role
in feature development.

## License

AGPL (\>=3)
