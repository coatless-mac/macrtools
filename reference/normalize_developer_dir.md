# Normalize a Developer Directory Path

Normalize a Developer Directory Path

## Usage

``` r
normalize_developer_dir(path)
```

## Arguments

- path:

  A bundle path or developer directory.

## Value

The normalized developer directory.

## Details

`xcode-select --switch` accepts either an `Xcode.app` bundle or a
developer directory inside one, so both spellings are normalized to the
developer directory. Trailing slashes and symlinks are resolved too,
because macOS reports paths in forms that
[`base::identical()`](https://rdrr.io/r/base/identical.html) will never
match.
