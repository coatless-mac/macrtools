# Changelog

## macrtools 0.0.8

### Breaking changes

- [`is_xcode_cli_installed()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-cli.md)
  now reports whether the Command Line Tools package is installed,
  rather than whether it is the selected developer directory. Use
  [`developer_dir()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-toolchain.md)
  or
  [`is_xcode_toolchain_usable()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-toolchain.md)
  to ask about the selection.

- [`is_xcode_app_installed()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-app-ide.md)
  now reports whether any Xcode is installed anywhere, rather than
  whether `/Applications/Xcode.app` is selected.

- [`xcode_cli_switch()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-cli.md)
  takes `path` as its first argument, where `password` used to be.

### New features

- Added support for macOS 27 (Golden Gate).

- [`xcode_toolchains()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-toolchain.md)
  lists every developer directory `xcode-select` can be pointed at, with
  its version and whether it is active.

- [`is_xcode_toolchain_usable()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-toolchain.md)
  reports whether a compiler is reachable through the active developer
  directory, which is what decides whether R can build a package.

- [`developer_dir()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-toolchain.md)
  resolves the active developer directory, honoring `DEVELOPER_DIR`.

- [`xcode_cli_version()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-cli.md)
  reports the installed Command Line Tools version.

- [`xcode_cli_switch()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-cli.md)
  gained a `path` argument, so a toolchain from
  [`xcode_toolchains()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-toolchain.md)
  can be selected instead of always forcing the Command Line Tools.

### Bug fixes

- Xcode is detected outside its default location. Both predicates
  compared the active developer directory against a hardcoded path, so a
  versioned bundle such as `/Applications/Xcode_16.4.app` reported
  nothing installed and triggered a needless `softwareupdate` run.

- macOS 27 is recognized as supported. The version ceiling is exclusive,
  so the previous bound rejected every 27.x release and aborted
  [`macos_rtools_install()`](https://mac.thecoatlessprofessor.com/macrtools/reference/macos-rtools.md),
  [`gfortran_install()`](https://mac.thecoatlessprofessor.com/macrtools/reference/gfortran.md)
  and
  [`openmp_install()`](https://mac.thecoatlessprofessor.com/macrtools/reference/openmp.md).

- macOS versions ending in `.0` compare correctly. `sw_vers` reports
  only two components for those releases, which placed each one in the
  previous release’s range: `"12.0"` read as Big Sur, `"11.0"` matched
  nothing, and `"10.13"` was rejected as unsupported.

- [`xcode_cli_install()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-cli.md)
  no longer skips installation when an unselected Xcode exists elsewhere
  on disk, since it provides no compiler.

- [`macos_rtools_install()`](https://mac.thecoatlessprofessor.com/macrtools/reference/macos-rtools.md)
  reports which toolchain is active instead of assuming a full Xcode,
  and reports a failure rather than success when nothing can compile.

- [`macos_rtools_uninstall()`](https://mac.thecoatlessprofessor.com/macrtools/reference/macos-rtools.md)
  no longer reports a component as removed when it was not installed.

- The Command Line Tools version no longer comes from
  `xcode-select --version`, which reports the version of `xcode-select`
  itself.

- [`recipes_binary_install()`](https://mac.thecoatlessprofessor.com/macrtools/reference/recipes_binary_install.md)
  documentation now states that the `darwin` repository is chosen by
  Darwin kernel version, not macOS version. The two are no longer a
  fixed offset apart.

## macrtools 0.0.7

### Features

- Added support for R 4.6.0 on macOS.
  - `gfortran` continues to use the universal GNU Fortran 14.2 installer
    (unchanged from R 4.5).
  - On Apple Silicon, R 4.6 (macOS 14 Sonoma and higher) installs the
    `recipes` binaries from the new `darwin23/arm64` repository. The
    repository is detected automatically and installs to the same
    `/opt/R/arm64` prefix as before, so no path changes are required.
- Updated the bundled LLVM OpenMP runtime from 19.1.0 to 19.1.5 to match
  the current release on <https://mac.r-project.org/openmp/>. The 19.1.x
  runtime covers Apple clang 1700.x and up (Xcode 16.3 through Xcode
  26.x, the R 4.6 Apple Silicon toolchain), so existing systems remain
  correctly matched.

### Internal

- Centralized the supported R version window behind
  [`minimum_supported_r_version()`](https://mac.thecoatlessprofessor.com/macrtools/reference/supported_r_version.md)
  /
  [`maximum_supported_r_version()`](https://mac.thecoatlessprofessor.com/macrtools/reference/supported_r_version.md)
  and replaced the repeated `is_r_version("4.x") || ...` chains with
  range checks
  ([`is_r_version_at_least()`](https://mac.thecoatlessprofessor.com/macrtools/reference/is_r_version_at_least.md),
  [`is_r_version_supported()`](https://mac.thecoatlessprofessor.com/macrtools/reference/is_r_version_supported.md)).
  Supporting a future R release that keeps the existing toolchain is now
  a one-line change.
- [`gfortran_update()`](https://mac.thecoatlessprofessor.com/macrtools/reference/gfortran.md)
  now requires “R 4.2 or above” (matching its documentation) rather than
  an explicit, hard-coded list of versions.

## macrtools 0.0.6.2

### Features

- Added support for macOS Tahoe (26.x).
- Fixed the macOS version check so Tahoe (26.x) is recognized as
  supported.
  ([\#28](https://github.com/coatless-mac/macrtools/issues/28))

## macrtools 0.0.6.1

### Features

- Added `openmp_*()` to install the LLVM OpenMP library for macOS.
  ([\#10](https://github.com/coatless-mac/macrtools/issues/10))
  - [`openmp_install()`](https://mac.thecoatlessprofessor.com/macrtools/reference/openmp.md)
    to install the OpenMP library and set flags in `~/.R/Makevars`.
  - [`openmp_uninstall()`](https://mac.thecoatlessprofessor.com/macrtools/reference/openmp.md)
    to uninstall the OpenMP library and set flags in `~/.R/Makevars`.
  - [`openmp_test()`](https://mac.thecoatlessprofessor.com/macrtools/reference/openmp.md)
    for detailed overview of OpenMP install.
  - [`is_openmp_installed()`](https://mac.thecoatlessprofessor.com/macrtools/reference/openmp.md)
    to check if the OpenMP library is installed.

### Bug Fixes

- Fixed
  [`macrtools::macos_rtools_install()`](https://mac.thecoatlessprofessor.com/macrtools/reference/macos-rtools.md)
  erroring during the summary portion.

## macrtools 0.0.6

### Features

- Updated to support R 4.5.0 on macOS.
  - Downloads and sets up the latest version of gfortran 14.2

## macrtools 0.0.5

### Features

- Updated to support macOS Sequoia (15.0.0)

## macrtools 0.0.4

### Features

- Updated to support R 4.4.0 on macOS.

### Deployment

- GitHub Actions now tests the package on `release` and
  [`next`](https://rdrr.io/r/base/Control.html) versions of macOS R’s
  version.

## macrtools 0.0.3

### Breaking change

- We’ve loosened the requirement to have Xcode CLI installed. The
  package will now accept if you have the Xcode.app IDE installed.
  - Note: The Xcode.app IDE requires significantly more space compared
    to Xcode CLI. We highly suggest that you install Xcode CLI.

### Features

- Added
  [`is_xcode_app_installed()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-app-ide.md)
  to check to see if the full Xcode.app IDE is present.
- Added
  [`xcodebuild()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcodebuild.md)
  to understand properties about the Xcode.app IDE installation.

### Changes

- Updated the supported version of macOS to macOS Sonoma (14.0.0).
- Improved the error message when the version of macOS is not supported.
- [`xcode_cli_install()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-cli.md)
  and
  [`macos_rtools_install()`](https://mac.thecoatlessprofessor.com/macrtools/reference/macos-rtools.md)
  have been modified to skip the installation of Xcode CLI if the full
  Xcode.app IDE is detected.

### Documentation

- Switched describing the ARM Mac entries from M1 or M2 to M-series to
  generalize with how Apple names software.
- Improved details regarding the paths being chosen for software
  installed.

### Deployment

- Updated the GitHub Actions for both R-CMD-check and pkgdown.

## macrtools 0.0.2

### Features

- Support the toolchain compilation requirements for 4.3.z series
- Updated the gfortran binary to the universal v12.2 Fortran installer
  across both Intel and arm64 platforms.
  - The gfortran path placed into `~/.Renviron` is now
    `/opt/gfortran/bin/gfortran`.
- Recipes now supports the `darwin20/x86_64` version (macOS 11) for
  Intel.
  - New installation directories for recipes downloads are given as:

| Name            | Installation Location | Target                      |
|-----------------|-----------------------|-----------------------------|
| darwin17/x86_64 | /usr/local            | macOS 10.13, Intel (x86_64) |
| darwin20/arm64  | /opt/R/arm64          | macOS 11, Apple M1 (arm64)  |
| darwin20/x86_64 | /opt/R/x86_64         | macOS 11, Intel (x86_64)    |

## macrtools 0.0.1

[![Video: Installing and using the {macrtools} package to setup the R
Compilation Toolchain for
macOS](http://img.youtube.com/vi/_fckF0fefXQ/0.jpg)](https://www.youtube.com/watch?v=_fckF0fefXQ)

### Features

- Automatic compiled code toolchain installation on macOS for R 4.0 -
  4.2
  - In R, type into console:
    [`macrtools::macos_rtools_install()`](https://mac.thecoatlessprofessor.com/macrtools/reference/macos-rtools.md)
- Install specific components of macOS compiled toolchain are available
  through:
  - Install Xcode CLI Tools with:
    [`macrtools::xcode_cli_install()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-cli.md)
  - Install gfortran with:
    [`macrtools::gfortran_install()`](https://mac.thecoatlessprofessor.com/macrtools/reference/gfortran.md)
  - Install precompiled binaries from the [`recipes` for macOS
    project](https://github.com/R-macos/recipes) with
    `macrtools::recipes_binary_install('r-base-dev')`
- Check if toolchains components are available:
  - Check if Xcode CLI Tools is installed with:
    [`macrtools::is_xcode_cli_installed()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-cli.md)
  - Check if gfortran is installed with:
    [`macrtools::is_gfortran_installed()`](https://mac.thecoatlessprofessor.com/macrtools/reference/gfortran.md)
- Uninstall toolchains components when they are no longer needed:
  - Uninstall Xcode CLI Tools with:
    [`macrtools::xcode_cli_uninstall()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-cli.md)
  - Uninstall gfortran with:
    [`macrtools::gfortran_uninstall()`](https://mac.thecoatlessprofessor.com/macrtools/reference/gfortran.md)
