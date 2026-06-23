# Find, Install, or Uninstall OpenMP

Set of functions that seek to identify whether OpenMP was installed,
allow OpenMP to be installed, and removing OpenMP.

## Usage

``` r
is_openmp_installed()

openmp_version()

openmp_install(
  password = base::getOption("macrtools.password"),
  verbose = TRUE,
  configure_makevars = TRUE
)

openmp_uninstall(
  password = base::getOption("macrtools.password"),
  verbose = TRUE,
  configure_makevars = TRUE
)

openmp_test()
```

## Arguments

- password:

  Password for user account to install software. Default is `NULL`.

- verbose:

  Display messages indicating status

- configure_makevars:

  Automatically configure ~/.R/Makevars with or without OpenMP flags.
  Default is `TRUE`.

## Details

The `openmp_*()` suite of functions attempts to locate, install, and
uninstall the OpenMP runtime library based on the installed Xcode
version. These functions mirror the logic described on the [macOS R
Project OpenMP page](https://mac.r-project.org/openmp/) and should be
used by advanced users who wish to compile R packages that use OpenMP
for parallel processing. They are not required for most users.

OpenMP runtime is downloaded from the R-project repository for macOS and
installed to:

- Library: `/usr/local/lib/libomp.dylib`

- Headers:

  - `/usr/local/include/omp.h`;

  - `/usr/local/include/ompt.h`;

  - `/usr/local/include/omp-tools.h`; and,

  - `/usr/local/include/ompx.h` (added in LLVM 19.1.0).

**Note:** Apple has explicitly disabled OpenMP support in Xcode
compilers, but the runtime library can be installed separately and used
with `-Xclang -fopenmp` compiler flags.

## Check if `OpenMP` is installed

Checks the local file system for whether OpenMP runtime library is
installed in the default installation location. Specifically, it checks
for the `libomp.dylib` library file and the `omp.h` header file, e.g:

    # Check if OpenMP library exists
    [ -f /usr/local/lib/libomp.dylib ] && echo "Library found" || echo "Library not found"
    # Check if OpenMP header files exist
    [ -f /usr/local/include/omp.h ] && echo "Headers found" || echo "Headers not found"

## Installing `OpenMP`

The `openmp_install()` function aims to install the appropriate OpenMP
runtime library based on the detected Xcode version.

### OpenMP Installation Process

The installation process automatically detects your Xcode version and
downloads the corresponding OpenMP runtime from the R-project
repository:

    VERSION="19.1.5"

    # Download the appropriate version
    curl -O https://mac.r-project.org/openmp/openmp-${VERSION}-darwin20-Release.tar.gz

    # Install to system directories
    sudo tar fvxz openmp-${VERSION}-darwin20-Release.tar.gz -C /

### Using OpenMP in R Packages

During installation, we will automatically configure your
`~/.R/Makevars` file to include the necessary compiler flags for OpenMP
support.

    # macrtools - OpenMP: start
    CPPFLAGS += -Xclang -fopenmp
    LDFLAGS += -lomp
    # macrtools - OpenMP: end

Alternatively, you can manually add the lines to your `~/.R/Makevars`
file. Or, install packages from command line with:

    PKG_CPPFLAGS='-Xclang -fopenmp' PKG_LIBS=-lomp R CMD INSTALL myPackage

## Uninstalling `OpenMP`

The `openmp_uninstall()` attempts to remove OpenMP from the default
installation locations.

We use the *R* sanitized *shell* command to remove the installed files:

    sudo rm -f /usr/local/lib/libomp.dylib
    sudo rm -f /usr/local/include/omp.h
    sudo rm -f /usr/local/include/ompt.h
    sudo rm -f /usr/local/include/omp-tools.h
    sudo rm -f /usr/local/include/ompx.h

**Note:** `ompx.h` was included in LLVM 19.1.0 and may not exist in
older OpenMP versions.

## Testing OpenMP Installation

After installing OpenMP, you can test if it's working by checking the
`~/.R/Makevars` file for the correct flags and verifying the library
signature.

**Note:** This function does not attempt to compile any code, it only
checks the configuration and library signature.

## Examples

``` r
# Check if OpenMP is installed
is_openmp_installed()
#> [1] FALSE
```
