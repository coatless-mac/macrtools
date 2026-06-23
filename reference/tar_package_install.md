# Install Binary Package in a Tar Format

The use of `sudo` is required to unpack the binary file into the
low-level system area.

## Usage

``` r
tar_package_install(
  path_to_tar,
  install_directory,
  strip_levels,
  sudo = TRUE,
  password = NULL,
  verbose = TRUE
)
```

## Arguments

- path_to_tar:

  Location of where the tar file is

- install_directory:

  Location of where to unpack or extract the tar file.

- strip_levels:

  Remove nesting inside of the `tar` file

- sudo:

  Run the command as `root` through `sudo`. Default `TRUE`

- password:

  User password to use to enter `sudo` mode.

- verbose:

  Display status messages

## Details

Unpacks the Tar package and places it into a system library
