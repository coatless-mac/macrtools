# Supported R Version Window

Single source of truth for the oldest and newest R minor versions whose
macOS toolchain `macrtools` supports.

## Usage

``` r
minimum_supported_r_version()

maximum_supported_r_version()
```

## Value

A `"major.minor"` version string.

## Details

To validate support for a new R release, bump
`maximum_supported_r_version()`. A new toolchain *branch* (e.g. in
[`gfortran_install()`](https://mac.thecoatlessprofessor.com/macrtools/reference/gfortran.md)
or the `installers.R` helpers) is only required when the toolchain
itself changes for that release; the range checks
([`is_r_version_at_least()`](https://mac.thecoatlessprofessor.com/macrtools/reference/is_r_version_at_least.md))
otherwise carry the newest tier forward automatically.
