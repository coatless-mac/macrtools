# Detect if the Xcode.app IDE is Installed

Checks whether an Xcode.app Integrated Developer Environment (IDE) is
installed anywhere on the system.

## Usage

``` r
is_xcode_app_installed()
```

## Details

Installed, not *selected*. Xcode is routinely installed under a
versioned name such as `/Applications/Xcode_26.6.app`, which is what
every GitHub-hosted macOS runner does, and `man xcode-select` documents
non-default locations as supported. Bundles are therefore located by
bundle identifier rather than by path, so a renamed or relocated install
is still found.

To ask instead which toolchain is *active*, use
[`developer_dir()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-toolchain.md);
to ask whether a compiler can actually be reached, use
[`is_xcode_toolchain_usable()`](https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-toolchain.md).

## Examples

``` r
# Check if Xcode.app IDE is on the path
is_xcode_app_installed()
#> [1] TRUE
```
