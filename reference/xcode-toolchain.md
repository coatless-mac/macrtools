# Inspect the Active Xcode Toolchain

Report which developer directory is active, whether a compiler can
actually be reached through it, and which toolchains are available to
switch to.

## Usage

``` r
developer_dir()

is_xcode_toolchain_usable()

xcode_toolchains()
```

## Value

The active developer directory, or `NA_character_` when nothing usable
is selected.

TRUE if a usable compiler is reachable, FALSE otherwise

A data frame with one row per selectable developer directory and columns
`path`, `type` (`"cli"` or `"app"`), `version` and `active`.

## Details

`developer_dir()` resolves the active developer directory.
`DEVELOPER_DIR` overrides the selection stored by `xcode-select`, for
`xcrun`, the `/usr/bin` shims and `xcodebuild` alike, so it is consulted
first. Note that `xcode-select --print-path` echoes its value and exits
`0` even when the directory does not exist, so the exit status alone
proves nothing; the path is verified before being returned.

This is the question that matters for building R packages. R is
configured with a bare `clang`, which resolves to the `/usr/bin` shim,
so whether a package can be compiled depends on the active developer
directory dispatching successfully, not on where the tools live.

`xcrun --find clang` resolves through the active directory and honors
`DEVELOPER_DIR`. It can, however, exit `0` while printing an error for a
degraded bundle, so the returned path is confirmed to exist and be
executable rather than trusting the exit status.

## Listing the Available Toolchains

`xcode_toolchains()` reports every developer directory `xcode-select`
could be pointed at: the Command Line Tools, plus each installed Xcode
bundle wherever it lives. Pass a `path` from this table to
[`xcode_cli_switch()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-cli.md).

Note that this lists *developer directories*, not `.xctoolchain`
bundles. Those are a separate axis selected through `xcrun --toolchain`
and the `TOOLCHAINS` variable, and are mostly Swift snapshots.

## Examples

``` r
# Which developer directory is active
developer_dir()
#> [1] "/Applications/Xcode_26.6.app/Contents/Developer"
# Check whether a compiler is actually reachable
is_xcode_toolchain_usable()
#> [1] TRUE
# List every toolchain xcode-select could use
xcode_toolchains()
#>                                                path type             version
#> 1 /Applications/Xcode_26.0.1.app/Contents/Developer  app              26.0.1
#> 2 /Applications/Xcode_26.1.1.app/Contents/Developer  app              26.1.1
#> 3   /Applications/Xcode_26.2.app/Contents/Developer  app                26.2
#> 4   /Applications/Xcode_26.3.app/Contents/Developer  app                26.3
#> 5 /Applications/Xcode_26.4.1.app/Contents/Developer  app              26.4.1
#> 6   /Applications/Xcode_26.5.app/Contents/Developer  app                26.5
#> 7   /Applications/Xcode_26.6.app/Contents/Developer  app                26.6
#> 8               /Library/Developer/CommandLineTools  cli 26.6.0.0.1781586589
#>   active
#> 1  FALSE
#> 2  FALSE
#> 3  FALSE
#> 4  FALSE
#> 5  FALSE
#> 6  FALSE
#> 7   TRUE
#> 8  FALSE
```
