# Install Binary Library from the CRAN R macOS Recipes Project

Convenience function that seeks to install pre-built binary libraries
used on [CRAN](https://cran.r-project.org/) for macOS through the
[recipes](https://github.com/s-u/recipes) system designed by Simon
Urbanek.

## Usage

``` r
recipes_binary_install(
  pkgs,
  url = "https://mac.R-project.org/bin",
  os = base::tolower(base::paste0(base::system("uname -s", intern = TRUE),
    base::gsub("\\..*", "", base::system("uname -r", intern = TRUE)))),
  arch = base::system("uname -m", intern = TRUE),
  os.arch = "auto",
  dependencies = TRUE,
  action = c("install", "list", "download"),
  sudo = TRUE,
  password = NULL,
  verbose = TRUE
)
```

## Arguments

- pkgs:

  Character vector of binary names to install, `"all"` for all binaries,
  or `"r-base-dev"` for *R* binaries.

- url:

  URL of the repository root. Default <https://mac.R-project.org/bin>

- os:

  Name and version of the OS, e.g. `"darwin22"` where `"darwin"` refers
  to macOS and `22` is the kernel version number.

- arch:

  The architecture of either `arm64` (M1/M2/M3) or `x86_64` (Intel).
  This is only used if `os.arch="auto"`.

- os.arch:

  Either name of the repository such as `"darwin20/arm64"`,
  `"darwin20/x86_64"`, `"darwin17/x86_64"`, or `"auto"`. Default
  `"auto"`.

- dependencies:

  Install build dependencies (`TRUE`) or only the requested packages
  (`FALSE`). Default `TRUE`.

- action:

  Determine if the binary should be downloaded and installed
  (`"install"`), displayed (`"list"`), or downloaded but not installed
  (`"download"`). Default `"install"` to download and install the
  binaries.

- sudo:

  Attempt to install the binaries using `sudo` permissions. Default
  `TRUE`.

- password:

  User password to switch into the `sudo` user. Default `NULL`.

- verbose:

  Describe the steps being taken. Default `TRUE`.

## Details

The function attempts to detect the appropriate repository and
installation path for the binary packages when `"auto"` is set. By
default, the repository and the install path are either:

|  |  |  |
|----|----|----|
| Name | Installation Location | Target |
| [darwin17/x86_64](https://mac.r-project.org/bin/darwin17/x86_64) | /usr/local | macOS 10.13, Intel (x86_64) |
| [darwin20/x86_64](https://mac.r-project.org/bin/darwin20/x86_64) | /opt/R/x86_64 | macOS 11, Intel (x86_64) |
| [darwin20/arm64](https://mac.r-project.org/bin/darwin20/arm64) | /opt/R/arm64 | macOS 11, Apple Silicon (arm64) |
| [darwin23/arm64](https://mac.r-project.org/bin/darwin23/arm64) | /opt/R/arm64 | macOS 14 (Sonoma), Apple Silicon (arm64), used by R 4.6 |

The correct repository is detected automatically: the highest `darwin`
version less than or equal to your macOS is selected for your
architecture. R 4.6 on Apple Silicon (macOS 14+) therefore resolves to
`darwin23/arm64`, which installs to the same `/opt/R/arm64` prefix as
`darwin20/arm64`.

## Differences

The official implementation uses `quiet` as a parameter to suppress
output instead of `verbose`.

## Author

Simon Urbanek wrote the function and made it available at
<https://mac.r-project.org/bin/>

James Joseph Balamuta packaged the function and added the option to use
`sudo` on the command line.

## Examples

``` r
# Perform a dry-run to see the required development packages.
recipes_binary_install("r-base-dev", action = "list")
#> Downloading https://mac.R-project.org/bin/REPOS ...
#> Using repository  https://mac.R-project.org/bin/darwin23/arm64 ...
#> Downloading index  https://mac.R-project.org/bin/darwin23/arm64/PACKAGES ...
#>  [1] "https://mac.R-project.org/bin/darwin23/arm64/xz-5.8.3-darwin.23-arm64.tar.xz"         
#>  [2] "https://mac.R-project.org/bin/darwin23/arm64/tiff-4.7.0-darwin.23-arm64.tar.xz"       
#>  [3] "https://mac.R-project.org/bin/darwin23/arm64/libpng-1.6.56-darwin.23-arm64.tar.xz"    
#>  [4] "https://mac.R-project.org/bin/darwin23/arm64/openssl-3.6.1-darwin.23-arm64.tar.xz"    
#>  [5] "https://mac.R-project.org/bin/darwin23/arm64/jpeg-9f-darwin.23-arm64.tar.xz"          
#>  [6] "https://mac.R-project.org/bin/darwin23/arm64/pcre2-10.47-darwin.23-arm64.tar.xz"      
#>  [7] "https://mac.R-project.org/bin/darwin23/arm64/cairo-1.17.6-darwin.23-arm64.tar.xz"     
#>  [8] "https://mac.R-project.org/bin/darwin23/arm64/texinfo-7.3-darwin.23-arm64.tar.xz"      
#>  [9] "https://mac.R-project.org/bin/darwin23/arm64/libdeflate-1.25-darwin.23-arm64.tar.xz"  
#> [10] "https://mac.R-project.org/bin/darwin23/arm64/zstd-1.5.7-darwin.23-arm64.tar.xz"       
#> [11] "https://mac.R-project.org/bin/darwin23/arm64/libwebp-1.6.0-darwin.23-arm64.tar.xz"    
#> [12] "https://mac.R-project.org/bin/darwin23/arm64/pkgconfig-0.29.2-darwin.23-arm64.tar.xz" 
#> [13] "https://mac.R-project.org/bin/darwin23/arm64/freetype-2.14.3-darwin.23-arm64.tar.xz"  
#> [14] "https://mac.R-project.org/bin/darwin23/arm64/fontconfig-2.14.2-darwin.23-arm64.tar.xz"
#> [15] "https://mac.R-project.org/bin/darwin23/arm64/pixman-0.42.2-darwin.23-arm64.tar.xz"    
#> [16] "https://mac.R-project.org/bin/darwin23/arm64/lz4-1.9.4-darwin.23-arm64.tar.xz"        
#> [17] "https://mac.R-project.org/bin/darwin23/arm64/sys-stubs-1.0-darwin.23-arm64.tar.xz"    
#> [18] "https://mac.R-project.org/bin/darwin23/arm64/harfbuzz-14.1.0-darwin.23-arm64.tar.xz"  
#> [19] "https://mac.R-project.org/bin/darwin23/arm64/expat-2.7.5-darwin.23-arm64.tar.xz"      
#> [20] "https://mac.R-project.org/bin/darwin23/arm64/icu-71.1-darwin.23-arm64.tar.xz"         
#> [21] "https://mac.R-project.org/bin/darwin23/arm64/fribidi-1.0.16-darwin.23-arm64.tar.xz"   

if (FALSE) { # \dontrun{
# Install the mandatory library binaries for building R on macOS using sudo
recipes_binary_install("r-base-dev", sudo = TRUE)
} # }
```
