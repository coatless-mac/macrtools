# Package index

## Main Installation Functions

Primary functions for installing and managing the complete R toolchain

- [`macos_rtools_install()`](https://mac.thecoatlessprofessor.com/macrtools/reference/macos-rtools.md)
  [`macos_rtools_uninstall()`](https://mac.thecoatlessprofessor.com/macrtools/reference/macos-rtools.md)
  : Install and Uninstall the macOS R Toolchain

## Xcode Command Line Tools

Functions for managing Xcode CLI installation and configuration

- [`is_xcode_cli_installed()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-cli.md)
  [`xcode_cli_path()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-cli.md)
  [`xcode_cli_install()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-cli.md)
  [`xcode_cli_uninstall()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-cli.md)
  [`xcode_cli_switch()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-cli.md)
  [`xcode_cli_reset()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-cli.md)
  : Find, Install, or Uninstall XCode CLI

- [`is_xcode_app_installed()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-app-ide.md)
  : Detect if the Xcode.app IDE is Installed

- [`xcode_select()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-select.md)
  [`xcode_select_path()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-select.md)
  [`xcode_select_version()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-select.md)
  :

  Interface with `xcode-select` Shell Commands

- [`xcodebuild()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcodebuild.md)
  [`xcodebuild_version()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcodebuild.md)
  :

  Interface with `xcodebuild` Shell Commands

## GNU Fortran Compiler (gfortran)

Functions for installing, updating, and managing gfortran

- [`is_gfortran_installed()`](https://mac.thecoatlessprofessor.com/macrtools/reference/gfortran.md)
  [`gfortran_version()`](https://mac.thecoatlessprofessor.com/macrtools/reference/gfortran.md)
  [`gfortran_install()`](https://mac.thecoatlessprofessor.com/macrtools/reference/gfortran.md)
  [`gfortran_uninstall()`](https://mac.thecoatlessprofessor.com/macrtools/reference/gfortran.md)
  [`gfortran_update()`](https://mac.thecoatlessprofessor.com/macrtools/reference/gfortran.md)
  : Find, Install, or Uninstall gfortran

## OpenMP

Functions for installing, updating, and managing OpenMP

- [`is_openmp_installed()`](https://mac.thecoatlessprofessor.com/macrtools/reference/openmp.md)
  [`openmp_version()`](https://mac.thecoatlessprofessor.com/macrtools/reference/openmp.md)
  [`openmp_install()`](https://mac.thecoatlessprofessor.com/macrtools/reference/openmp.md)
  [`openmp_uninstall()`](https://mac.thecoatlessprofessor.com/macrtools/reference/openmp.md)
  [`openmp_test()`](https://mac.thecoatlessprofessor.com/macrtools/reference/openmp.md)
  : Find, Install, or Uninstall OpenMP

## R Development Libraries

Functions for installing binary packages from the R-macOS recipes
project

- [`recipes_binary_install()`](https://mac.thecoatlessprofessor.com/macrtools/reference/recipes_binary_install.md)
  : Install Binary Library from the CRAN R macOS Recipes Project

## Installation Paths & Directories

Functions that return standard installation directories for different
architectures

- [`install_directory_x86_64()`](https://mac.thecoatlessprofessor.com/macrtools/reference/install-directory.md)
  [`install_directory_x86_64_r_version_4_3()`](https://mac.thecoatlessprofessor.com/macrtools/reference/install-directory.md)
  [`install_directory_arm64()`](https://mac.thecoatlessprofessor.com/macrtools/reference/install-directory.md)
  [`install_directory_xcode_cli()`](https://mac.thecoatlessprofessor.com/macrtools/reference/install-directory.md)
  [`install_directory_xcode_app()`](https://mac.thecoatlessprofessor.com/macrtools/reference/install-directory.md)
  : Installation Directory Locations

## Binary Package Utilities

Lower-level functions for downloading and installing binary packages

- [`binary_download()`](https://mac.thecoatlessprofessor.com/macrtools/reference/binary_download.md)
  : Download Binary Packages
- [`tar_package_install()`](https://mac.thecoatlessprofessor.com/macrtools/reference/tar_package_install.md)
  : Install Binary Package in a Tar Format

## System Assertions & Validation

Functions to validate system requirements and compatibility

- [`assert()`](https://mac.thecoatlessprofessor.com/macrtools/reference/assert.md)
  [`assert_mac()`](https://mac.thecoatlessprofessor.com/macrtools/reference/assert.md)
  [`assert_macos_supported()`](https://mac.thecoatlessprofessor.com/macrtools/reference/assert.md)
  [`assert_aarch64()`](https://mac.thecoatlessprofessor.com/macrtools/reference/assert.md)
  [`assert_x86_64()`](https://mac.thecoatlessprofessor.com/macrtools/reference/assert.md)
  [`assert_r_version_supported()`](https://mac.thecoatlessprofessor.com/macrtools/reference/assert.md)
  : Assert a condition

## Utility Functions

Helper functions and utilities

- [`print(`*`<cli>`*`)`](https://mac.thecoatlessprofessor.com/macrtools/reference/print.cli.md)
  : Print CLI Responses
