# Check if the macOS Version Falls in a Range

Check if the macOS Version Falls in a Range

## Usage

``` r
macos_version_in_range(lower, upper)
```

## Arguments

- lower:

  Lower bound for the macOS version (inclusive).

- upper:

  Upper bound for the macOS version (exclusive).

## Value

TRUE if the running macOS version is in `[lower, upper)`.

## Details

The running version and both bounds are padded by
[`pad_version()`](https://mac.thecoatlessprofessor.com/macrtools/reference/pad_version.md)
before comparison, so a bound may be written with two or three
components interchangeably.
