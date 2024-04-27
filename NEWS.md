# macrtools 0.0.4

## Features

- Updated to support R 4.4.0 on macOS.

## Deployment

- GitHub Actions now tests the package on `release` and `next` versions of 
  macOS R's version.

# macrtools 0.0.3

## Breaking change

- We've loosened the requirement to have Xcode CLI installed. The package will
  now accept if you have the Xcode.app IDE installed. 
  - Note: The Xcode.app IDE requires significantly more space compared to
    Xcode CLI. We highly suggest that you install Xcode CLI.

## Features

- Added `is_xcode_app_installed()` to check to see if the full Xcode.app IDE
  is present.
- Added `xcodebuild()` to understand properties about the Xcode.app IDE 
  installation.

## Changes

- Updated the supported version of macOS to macOS Sonoma (14.0.0).
- Improved the error message when the version of macOS is not supported.
- `xcode_cli_install()` and `macos_rtools_install()` have been modified to
  skip the installation of Xcode CLI if the full Xcode.app IDE is detected.

## Documentation

- Switched describing the ARM Mac entries from M1 or M2 to M-series to 
  generalize with how Apple names software.
- Improved details regarding the paths being chosen for software installed.

## Deployment

- Updated the GitHub Actions for both R-CMD-check and pkgdown.

# macrtools 0.0.2

## Features

- Support the toolchain compilation requirements for 4.3.z series 
- Updated the gfortran binary to the universal v12.2 Fortran installer across both Intel and arm64 platforms.
   - The gfortran path placed into `~/.Renviron` is now `/opt/gfortran/bin/gfortran`.
- Recipes now supports the `darwin20/x86_64` version (macOS 11) for Intel. 
  - New installation directories for recipes downloads are given as:  

Name | Installation Location | Target
-- | -- | --
darwin17/x86_64 | /usr/local | macOS 10.13, Intel (x86_64)
darwin20/arm64 | /opt/R/arm64 | macOS 11, Apple M1 (arm64)
darwin20/x86_64 | /opt/R/x86_64 | macOS 11, Intel (x86_64)


# macrtools 0.0.1

[![Video: Installing and using the `{macrtools}` package to setup the R Compilation Toolchain for macOS](http://img.youtube.com/vi/_fckF0fefXQ/0.jpg)](https://www.youtube.com/watch?v=_fckF0fefXQ)

## Features 

- Automatic compiled code toolchain installation on macOS for R 4.0 - 4.2
  - In R, type into console: `macrtools::macos_rtools_install()`
- Install specific components of macOS compiled toolchain are available through:
  - Install Xcode CLI Tools with: `macrtools::xcode_cli_install()`
  - Install gfortran with: `macrtools::gfortran_install()`
  - Install precompiled binaries from the [`recipes` for macOS project](https://github.com/R-macos/recipes)
    with `macrtools::recipes_binary_install('r-base-dev')`
- Check if toolchains components are available:
  - Check if Xcode CLI Tools is installed with: `macrtools::is_xcode_cli_installed()`
  - Check if gfortran is installed with: `macrtools::is_gfortran_installed()`
- Uninstall toolchains components when they are no longer needed:
  - Uninstall Xcode CLI Tools with: `macrtools::xcode_cli_uninstall()`
  - Uninstall gfortran with: `macrtools::gfortran_uninstall()`
