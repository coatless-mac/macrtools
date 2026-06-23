# Find, Install, or Uninstall gfortran

Set of functions that seek to identify whether gfortran was installed,
allow gfortran to be installed, and removing gfortran.

## Usage

``` r
is_gfortran_installed()

gfortran_version()

gfortran_install(
  password = base::getOption("macrtools.password"),
  verbose = TRUE
)

gfortran_uninstall(
  password = base::getOption("macrtools.password"),
  verbose = TRUE
)

gfortran_update(
  password = base::getOption("macrtools.password"),
  verbose = TRUE
)
```

## Arguments

- password:

  Password for user account to install software. Default is `NULL`.

- verbose:

  Display messages indicating status

## Details

The `gfortran` suite of functions attempts to locate, install, and
uninstall `gfortran` based the default installation locations that
depend on architecture:

- Intel (`x86_64`) and M-series (`arm64` or `aarch64`) for R \>= 4.3

  - `/opt/gfortran/`

  - `/opt/gfortran/bin`

- Intel (`x86_64`) for R \< 4.3

  - `/usr/local/gfortran`

  - `/usr/local/bin/gfortran`

- M-series (`arm64` or `aarch64`) for 4.1 \<= R \< 4.3

  - `/opt/R/arm64/gfortran/`

  - `/opt/R/arm64/bin/gfortran`

## Check if `gfortran` is installed

Checks the local file system for whether `gfortran` is installed in the
default installation location.

## Installing `gfortran`

The `gfortran_install()` function aims to install `gfortran` into the
appropriate location for **Intel** (`x86_64`) or **M-series**
(`arm64`/`aarch64`) depending on the R version used.

### gfortran Installation for R 4.5-4.6

The `gfortran` installer for R 4.5-4.6 is a universal installer that
places `gfortran` into the `/opt/gfortran` for both **Intel** (`x86_64`)
and **M-series** (`arm64`/`aarch64`) macs. R 4.6 uses the same GNU
Fortran 14.2 universal compiler as R 4.5.

    # Install the downloaded package into root
    sudo installer \
      -pkg /path/to/gfortran-14.2-universal.pkg \
      -target /

Once installed, we modify the `PATH` environment variable to recognize
the newly installed software by adding into the `~/.Renviron` file the
following:

    touch ~/.Renviron
    cat << "EOF" > ~/.Renviron
    ## macrtools - gfortran: start
    PATH=${PATH}:/opt/gfortran/bin
    ## macrtools - gfortran: end
    EOF

### gfortran Installation for R 4.3-4.4

The `gfortran` installer for R 4.3-4.4 is a universal installer that
places `gfortran` into the `/opt/gfortran` for both **Intel** (`x86_64`)
and **M-series** (`arm64`/`aarch64`) macs.

    # Install the downloaded package into root
    sudo installer \
      -pkg /path/to/gfortran-12.2-universal.pkg \
      -target /

Once installed, we modify the `PATH` environment variable to recognize
the newly installed software by adding into the `~/.Renviron` file the
following:

    touch ~/.Renviron
    cat << "EOF" > ~/.Renviron
    ## macrtools - gfortran: start
    PATH=${PATH}:/opt/gfortran/bin
    ## macrtools - gfortran: end
    EOF

### gfortran Installation for Intel Macs (`x86_64`)

The Intel `gfortran` installer is a DMG image that is mounted,
installed, and unmounted. We're using the following set of *R* sanitized
*shell* commands:

    # Mount the `.dmg` installer image
    hdiutil attach "$path_to_dmg" -nobrowse -quiet

    # Install the package from DMG image into root
    sudo installer \
      -pkg /Volume/gfortran-8.2-Mojave/gfortran-8.2-Mojave/gfortran.pkg \
      -target /

    # Unmount the `.dmg` installer image
    hdiutil detach /Volumes/gfortran-8.2-Mojave

Lastly, we modify the `PATH` environment variable to recognize the newly
installed software by adding into the `~/.Renviron` file the following:

    touch ~/.Renviron
    cat << "EOF" > ~/.Renviron
    ## macrtools - gfortran: start
    PATH=${PATH}:/usr/local/gfortran/bin
    ## macrtools - gfortran: end
    EOF

### gfortran Installation for M-series Macs (`arm64`)

The **M-series** `gfortran` installer is a tar file that is unpacked
into the directory. Depending on the *R* version, we opt to install
either **gfortran 12 for R 4.2** or **gfortran 11 for R 4.1**

If users are on **R 4.2** with an **M-series** mac, then the *R*
sanitized *shell* commands used are:

    URL="https://mac.r-project.org/tools/gfortran-12.0.1-20220312-is-darwin20-arm64.tar.xz"
    curl -O --output-dir /tmp/ "$URL"
    sudo mkdir -p /opt/R/arm64/
    sudo tar fxz /tmp/gfortran-12.0.1-20220312-is-darwin20-arm64.tar.xz -C /opt/R/arm64/ --strip-components 3
    rm  /tmp/gfortran-12.0.1-20220312-is-darwin20-arm64.tar.xz

If users are on **R 4.1** with an **M-series** mac, then the *R*
sanitized *shell* commands used are:

    URL="https://mac.r-project.org/libs-arm64/gfortran-f51f1da0-darwin20.0-arm64.tar.gz"
    curl -O --output-dir /tmp/ "$URL"
    sudo mkdir -p /opt/R/arm64/
    sudo tar fxz /tmp/gfortran-f51f1da0-darwin20.0-arm64.tar.gz -C /opt/R/arm64/ --strip-components 3
    rm  /tmp/gfortran-f51f1da0-darwin20.0-arm64.tar.gz

Lastly, we modify the `PATH` environment variable to recognize the newly
installed software by adding into the `~/.Renviron` file the following:

    touch ~/.Renviron
    cat << "EOF" > ~/.Renviron
    ## macrtools - gfortran: start
    PATH=${PATH}:/opt/R/arm64/gfortran/bin
    ## macrtools - gfortran: end
    EOF

## Uninstalling `gfortran`

The `gfortran_uninstall()` attempts to remove `gfortran` from the
default installation locations described in the details section.

### Uninstalling gfortran for Intel Macs

We use the *R* sanitized *shell* command of:

    sudo rm -rf /usr/local/gfortran /usr/local/bin/gfortran

These uninstall steps are based on:

<https://gcc.gnu.org/wiki/GFortranBinariesMacOS>

### Uninstalling gfortran for M1 or M2 Macs (`arm64`)

For M1 or M2 Macs (`arm64`), we use the *R* sanitized *shell* command
of:

    sudo rm -rf /opt/R/arm64/gfortran/ /opt/R/arm64/bin/gfortran

This aligns with the default path used by CRAN for `arm64`:

<https://mac.r-project.org/tools/>

## Updating `gfortran`

The `gfortran_update()` attempts to update the version of `gfortran`
installed using the provided `gfortran-update-sdk` inside of
`/opt/R/arm64/gfortran/bin`.

Please be advised that the update command only works for M1/M2
(`arm64`/`aarch64`) users on *R* 4.2 or above.

The update command is issued using an *R* sanitized version of the
*shell* command:

    sudo /opt/R/arm64/gfortran/bin/gfortran-update-sdk

## Examples

``` r
# Check if gfortran is installed
is_gfortran_installed()
#> [1] TRUE
```
