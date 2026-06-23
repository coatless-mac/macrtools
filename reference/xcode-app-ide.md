# Detect if the Xcode.app IDE is Installed

Checks to see whether Xcode.app Integrated Developer Environment (IDE)
was installed and is active under the default location.

## Usage

``` r
is_xcode_app_installed()
```

## Details

Checks to see if Xcode.app IDE is active by running:

    xcode-select -p

If it returns:

    /Applications/Xcode.app/Contents/Developer

We consider the full Xcode.app to be **installed** and **selected** as
the command line tools to use.

## Examples

``` r
# Check if Xcode.app IDE is on the path
is_xcode_app_installed()
#> [1] FALSE
```
