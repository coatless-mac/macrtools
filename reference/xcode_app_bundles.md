# Locate Every Installed Xcode Bundle

Locate Every Installed Xcode Bundle

## Usage

``` r
xcode_app_bundles()
```

## Value

Paths to the installed Xcode bundles, possibly empty.

## Details

Xcode is routinely installed under a versioned name, which is what every
GitHub-hosted macOS runner does (`/Applications/Xcode_26.6.app`), and
`man xcode-select` documents non-default locations as supported.
Spotlight is queried by bundle identifier so the name and location do
not matter, with a glob fallback for machines where Spotlight indexing
is disabled. Time Machine copies are discarded.
