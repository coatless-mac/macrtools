# Installation Directory Locations

Pre-defined constants for each software install location.

## Usage

``` r
install_directory_x86_64()

install_directory_x86_64_r_version_4_3()

install_directory_arm64()

install_directory_xcode_cli()

install_directory_xcode_app()
```

## Value

A string containing a fixed path to the installation directory.

## Details

We have the following fixed paths:

- Intel (x86_64)

  - macOS 10.13: `/usr/local`

  - macOS 11: `/opt/R/x86_64`

- M1, M2, M3 (`arm64` or `aarch64`)

  - `/opt/R/arm64`

- Xcode Command Line Tools (Xcode CLI)

  - `/Library/Developer/CommandLineTools`

- Xcode App

  - `/Applications/Xcode.app/Contents/Developer`
