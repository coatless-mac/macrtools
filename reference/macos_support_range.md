# Human-readable macOS Support Window

Renders the supported macOS range for user-facing messages.

## Usage

``` r
macos_support_range()
```

## Value

A string naming the supported macOS range.

## Details

The upper major version is derived from
[`first_unsupported_macos_version()`](https://mac.thecoatlessprofessor.com/macrtools/reference/supported_macos_version.md)
rather than written out, so the advertised range cannot disagree with
the check it describes.
