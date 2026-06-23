# Find, Install, or Uninstall XCode CLI

Set of functions that seek to identify whether XCode CLI was installed,
allow XCode CLI to be installed, and removing XCode CLI.

## Usage

``` r
is_xcode_cli_installed()

xcode_cli_path()

xcode_cli_install(
  password = base::getOption("macrtools.password"),
  verbose = TRUE
)

xcode_cli_uninstall(
  password = base::getOption("macrtools.password"),
  verbose = TRUE
)

xcode_cli_switch(
  password = base::getOption("macrtools.password"),
  verbose = TRUE
)

xcode_cli_reset(
  password = base::getOption("macrtools.password"),
  verbose = TRUE
)
```

## Arguments

- password:

  User password to access `sudo`.

- verbose:

  Display status messages

## Check if XCode CLI is installed

Checks to see if Xcode CLI returns a viable path to the default Xcode
CLI location by checking the output of:

    xcode-select -p

## XCode CLI Installation

The `xcode_cli_install()` function performs a headless or
non-interactive installation of the Xcode CLI tools. This installation
process requires three steps:

1.  Place a temporary file indicating the need to download Xcode CLI

2.  Determine the latest version of Xcode CLI by running
    `softwareupdate`

3.  Install the latest version using `softwareupdate` with `sudo`.

The alternative approach would be an interactive installation of Xcode
CLI by typing into Terminal:

    sudo xcode-select --install

This command will trigger a pop up window that will walk through the
package installation.

### Steps of the Headless CLI Installation

The temporary file is created using:

    touch /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress

From there, we deduce the latest version of Xcode available to the user
through an *R* sanitized version of the chained *shell* commands:

    product_information=softwareupdate -l |
       grep '\\*.*Command Line' |
       tail -n 1 |
       awk -F"*" '{print $2}' |
       sed -e 's/^ *//' |
       sed 's/Label: //g' |
       tr -d '\n'

Then, we trigger the installation process with `sudo` using:

    sudo softwareupdate -i "$product_information" --verbose

where `$product_information` is obtained from the previous command.

Finally, we remove the temporary installation file.

    touch /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress

These steps were obtained from Timothy Sutton's
[xcode-cli-tools.sh](https://github.com/timsutton/osx-vm-templates/blob/ce8df8a7468faa7c5312444ece1b977c1b2f77a4/scripts/xcode-cli-tools.sh#L8-L14)
script and, slightly modernized.

## Uninstalling Xcode CLI

The `xcode_cli_uninstall()` attempts to remove *only* the Xcode CLI
tools.

Per the [Apple Technical Note
TN2339](https://developer.apple.com/library/archive/technotes/tn2339/_index.html#//apple_ref/doc/uid/DTS40014588-CH1-HOW_CAN_I_UNINSTALL_THE_COMMAND_LINE_TOOLS_):

- Xcode includes all of the command-line tools. If it is installed on
  your system, remove it to uninstall the command-line tools.

- If the `/Library/Developer/CommandLineTools` directory exists on your
  system, remove it to uninstall the command-line tools

Thus, the `xcode_cli_uninstall()` opts to perform the second step
**only**. We use an *R* sanitized *shell* version of:

    sudo rm -rf /Library/Developer/CommandLineTools

If the Xcode application is detect, we note that we did not uninstall
the Xcode application. Instead, we request the user uninstall the Xcode
app using the following steps:

1.  Make sure that Xcode is closed. Quit Xcode if needed.

2.  Open Finder \> Applications, select Xcode and move it to Trash.

3.  Empty the trash.

4.  In addition, open Terminal and run:

    sudo /Developer/Library/uninstall-devtools --mode=all

## Change Xcode CLI Location

If the Xcode Application has been previously installed, the underlying
path reported by `xcode-select` may not reflect the Xcode CLI location.
The situation can be rectified by using the `xcode_cli_switch()`
function, which changes the command line tool directory away from the
Xcode application location to the Xcode CLI location. This uses the
*default* Xcode CLI path.

    sudo xcode-select --switch /Library/Developer/CommandLineTools

If this does not fix the issue, we recommend using the
`xcode_cli_reset()` function.

## Reset Xcode CLI

The `xcode_cli_reset()` function uses `xcode-select` to restore the
default Xcode CLI settings.

We use an *R* sanitized *shell* version of:

    sudo xcode-select --reset

## Examples

``` r
# Check if Xcode CLI is installed
is_xcode_cli_installed()
#> [1] FALSE
# Determine the path location of Xcode CLI
xcode_cli_path()
#> [1] "/Applications/Xcode_16.4.app/Contents/Developer"
```
