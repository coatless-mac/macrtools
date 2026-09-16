# Check if a Developer Directory is a Full Xcode

Check if a Developer Directory is a Full Xcode

## Usage

``` r
developer_dir_is_app(dir = developer_dir())
```

## Arguments

- dir:

  Developer directory to test, defaulting to the active one.

## Value

TRUE if `dir` is a full Xcode developer directory, FALSE otherwise

## Details

A full Xcode carries the platform bundles; the Command Line Tools do
not. This distinguishes the two without shelling out to `xcodebuild`,
which is slow, errors when the Command Line Tools are active, and can
stall on the license agreement.
