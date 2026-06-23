# Check if the Running R Version is At Least a Target

Check if the Running R Version is At Least a Target

## Usage

``` r
is_r_version_at_least(target, version = r_version_major_minor())
```

## Arguments

- target:

  Target R version (e.g. `"4.3"`) to compare against.

- version:

  The version to test, defaults to the running R version.

## Value

TRUE if `version` is greater than or equal to `target`, FALSE otherwise
