# Pad a Version String to major.minor.patch

Pad a Version String to major.minor.patch

## Usage

``` r
pad_version(version)
```

## Arguments

- version:

  A version string such as `"27"`, `"27.0"` or `"27.0.1"`.

## Value

The version padded to at least three components.

## Details

`sw_vers -productVersion` reports only two components for a `.0`
release: macOS 27.0 reports `"27.0"`, never `"27.0.0"`.
[`utils::compareVersion()`](https://rdrr.io/r/utils/compareVersion.html)
ranks a shorter string *below* an otherwise-equal longer one, so
`compareVersion("10.13", "10.13.0")` is `-1`. Comparing an unpadded
two-component version against three-component bounds therefore places
every `.0` release in the *previous* release's range.

Padding both the version and the bounds to three components removes that
ambiguity. Versions with more than three components are left untouched.
