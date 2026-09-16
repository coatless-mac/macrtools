# Supported macOS Version Window

Single source of truth for the macOS versions whose toolchain
`macrtools` supports.

## Usage

``` r
minimum_supported_macos_version()

first_unsupported_macos_version()
```

## Value

A macOS version string.

## Details

`first_unsupported_macos_version()` is an **exclusive** bound: it names
the first macOS version that is *rejected*, not the newest one that is
accepted. Supporting a new macOS release therefore means setting it to
the *following* major version, e.g. supporting macOS 27 requires
`"28.0"`.

Naming the newest supported release here instead silently excludes that
entire release. That mistake shipped twice: once for Tahoe
([\#28](https://github.com/coatless-mac/macrtools/issues/28)) and again
for macOS 27.
