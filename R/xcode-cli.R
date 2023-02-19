#' @include utils.R shell.R installers.R
NULL

#' Find, Install, or Uninstall XCode CLI
#'
#' Set of functions that seek to identify whether XCode CLI was installed,
#' allow XCode CLI to be installed, and removing XCode CLI.
#'
#' @section Check if XCode CLI is installed:
#'
#' Checks to see if Xcode CLI returns a viable path to the default Xcode CLI
#' location by checking the output of:
#'
#' ```sh
#' xcode-select -p
#' ```
#'
#' @rdname xcode-cli
#' @export
#' @examples
#' # Check if Xcode CLI is installed
#' is_xcode_cli_installed()
is_xcode_cli_installed = function() {
    assert_mac()

    path_info = xcode_select_path()

    identical(path_info$status, 0L) &&
    identical(path_info$output, install_directory_xcode_cli()) &&
    dir.exists(path_info$output)
}

#' @section XCode CLI Installation:
#'
#' The `xcode_cli_install()` function performs a headless or non-interactive
#' installation of the Xcode CLI tools. This installation process requires
#' three steps:
#'
#' 1. Place a temporary file indicating the need to download Xcode CLI
#' 2. Determine the latest version of Xcode CLI by running `softwareupdate`
#' 3. Install the latest version using `softwareupdate` with `sudo`.
#'
#' The alternative approach would be an interactive installation of Xcode CLI
#' by typing into Terminal:
#'
#' ```sh
#' sudo xcode-select --install
#' ```
#'
#' This command will trigger a pop up window that will walk through the
#' package installation.
#'
#' ### Steps of the Headless CLI Installation
#'
#' The temporary file is created using:
#'
#' ```sh
#' touch /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress
#' ```
#'
#' From there, we deduce the latest version of Xcode available to the user
#' through an _R_ sanitized version of the chained _shell_ commands:
#'
#' ```sh
#' product_information=softwareupdate -l |
#'    grep '\\*.*Command Line' |
#'    tail -n 1 |
#'    awk -F"*" '{print $2}' |
#'    sed -e 's/^ *//' |
#'    sed 's/Label: //g' |
#'    tr -d '\n'
#' ```
#'
#' Then, we trigger the installation process with `sudo` using:
#'
#' ```sh
#' sudo softwareupdate -i "$product_information" --verbose
#' ```
#'
#' where `$product_information` is obtained from the previous command.
#'
#' Finally, we remove the temporary installation file.
#'
#' ```sh
#' touch /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress
#' ```
#'
#' These steps were obtained from Timothy Sutton's
#' [xcode-cli-tools.sh](https://github.com/timsutton/osx-vm-templates/blob/ce8df8a7468faa7c5312444ece1b977c1b2f77a4/scripts/xcode-cli-tools.sh#L8-L14)
#' script and, slightly modernized.
#'
#' @export
#' @rdname xcode-cli
#' @param verbose    Display status messages
xcode_cli_install = function(password = getOption("macrtools.password"), verbose = TRUE){
    assert_mac()
    if(isTRUE(is_xcode_cli_installed())) {

        if(verbose) message("Xcode CLI is already installed.")

        return( invisible(TRUE) )
    }

    temporary_xcli_file = "/tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress"

    # Create a temporary in-progress file
    file.create(temporary_xcli_file)

    product_information =
        system("softwareupdate -l |
          grep '\\*.*Command Line' |
          tail -n 1 |
          awk -F\"*\" '{print $2}' |
          sed -e 's/^ *//' |
          sed 's/Label: //g' |
          tr -d '\n'", intern = TRUE)

    if (verbose) {
        cat("Attempting to install Xcode CLI version:", product_information, "...\n")
        cat("Please be patient or grab a cup of coffee as Xcode CLI installs.\n")
        cat("\nWe expect the installation to take between 10 to 15 minutes.\n\n")
    }

    user_password = password
    describe_everything = isTRUE(verbose)

    cmd = paste("softwareupdate", "-i", shQuote(product_information), "--verbose")

    xcli_status = shell_execute(cmd,
                           sudo = TRUE, password = user_password, verbose = describe_everything)

    # Remove temporary in-progress file if left in place
    if(file.exists(temporary_xcli_file)) {
        file.remove(temporary_xcli_file)
    }

    xcli_clean = identical(xcli_status, 0L)


    if(!isTRUE(xcli_clean)) {
        cat("We were not able to install Xcode CLI ...\n")
        cat("Please try to manually install using: ..\n")
        cat("https://rmacoslib.github.io/macrtools/reference/xcode-cli.html#xcode-cli-installation\n")
        return(invisible(FALSE))
    }

    return( invisible( xcli_clean ) )
}



#' @section Uninstalling Xcode CLI:
#'
#' The `xcode_cli_uninstall()` attempts to remove _only_ the Xcode CLI tools.
#'
#' Per the [Apple Technical Note TN2339](https://developer.apple.com/library/archive/technotes/tn2339/_index.html#//apple_ref/doc/uid/DTS40014588-CH1-HOW_CAN_I_UNINSTALL_THE_COMMAND_LINE_TOOLS_):
#'
#' - Xcode includes all of the command-line tools. If it is installed on your system, remove it to uninstall the command-line tools.
#' - If the `/Library/Developer/CommandLineTools` directory exists on your system, remove it to uninstall the command-line tools
#'
#' Thus, the `xcode_cli_uninstall()` opts to perform the second step **only**.
#' We use an _R_ sanitized _shell_ version of:
#'
#' ```sh
#' sudo rm -rf /Library/Developer/CommandLineTools
#' ```
#'
#' If the Xcode application is detect, we note that we did not uninstall the
#' Xcode application. Instead, we request the user uninstall the Xcode
#' app using the following steps:
#'
#' 1. Make sure that Xcode is closed. Quit Xcode if needed.
#' 2. Open Finder > Applications, select Xcode and move it to Trash.
#' 3. Empty the trash.
#' 4. In addition, open Terminal and run:
#'
#' ```sh
#' sudo /Developer/Library/uninstall-devtools --mode=all
#' ```
#'
#' @export
#' @rdname xcode-cli
#' @param password   User password to access `sudo`.
xcode_cli_uninstall = function(password = getOption("macrtools.password"), verbose = TRUE){
    assert_mac()
    if(isFALSE(is_xcode_cli_installed())) {
        if(verbose) message("Xcode CLI is not installed.")
        return( invisible(TRUE) )
    }

    xcli_path = xcode_cli_path()

    # We should never hit this line of code as the is_xcode_cli_installed() now
    # focuses on only CLI. But, we might want to change that.
    if(xcli_path == install_directory_xcode_app()) {
        message("We detected the full Xcode application in use at: ", xcode_cli_path)
        message("Please uninstall it using the App store.")
        return( invisible(TRUE) )
    }

    # Remove the shell execution script
    xcli_uninstall_status = shell_execute("rm -rf /Library/Developer/CommandLineTools",
                  sudo = TRUE,
                  password = password)

    xcli_uninstall_clean = identical(xcli_uninstall_status, 0L)

    invisible(xcli_uninstall_clean)
}

#' @rdname xcode-cli
#' @export
#' @examples
#' # Determine the path location of Xcode CLI
#' xcode_cli_path()
xcode_cli_path = function() {
    inquiry_on_path = xcode_select_path()
    if (identical(inquiry_on_path$status, 0L)) {
        inquiry_on_path$output
    } else {
        ""
    }
}

#' @section Change Xcode CLI Location:
#' If the Xcode Application has been previously installed, the underlying
#' path reported by `xcode-select` may not reflect the Xcode CLI location.
#' The situation can be rectified by using the [xcode_cli_switch()] function,
#' which changes the command line tool directory away from the Xcode application
#' location to the Xcode CLI location. This uses the _default_ Xcode CLI
#' path.
#'
#' ```sh
#' sudo xcode-select --switch /Library/Developer/CommandLineTools
#' ```
#'
#' If this does not fix the issue, we recommend using the [xcode_cli_reset()]
#' function.
#' @export
#' @rdname xcode-cli
xcode_cli_switch = function(password = getOption("macrtools.password"), verbose = TRUE) {

    # The path matches with the default install directory location.
    if (xcode_cli_path() == install_directory_xcode_cli()) {
        if (verbose) cat("Xcode CLI path is correctly set ...\n")
        return( invisible(TRUE) )
    }

    # Need to modify the location to the current CLI path
    if (verbose) cat("Setting the Xcode CLI path ...\n")

    cmd = paste("xcode-select", "--switch", install_directory_xcode_cli())

    # Change the directory
    xcli_switch_status = shell_execute(cmd,
                           sudo = TRUE,
                           password = password,
                           verbose = verbose)

    xcli_switch_clean = identical(xcli_switch_status, 0L)

    return(invisible(xcli_switch_clean))
}

#' @section Reset Xcode CLI:
#' The [xcode_cli_reset()] function uses `xcode-select` to restore
#' the default Xcode CLI settings.
#'
#' We use an _R_ sanitized _shell_ version of:
#'
#' ```sh
#' sudo xcode-select --reset
#' ```
#'
#' @export
#' @rdname xcode-cli
xcode_cli_reset = function(password = getOption("macrtools.password"), verbose = TRUE) {

    if (verbose) cat("Attempting to reset Xcode CLI to default settings ...\n")

    cmd = paste("xcode-select", "--reset")

    # Change the directory
    xcli_reset_status = shell_execute(cmd,
                           sudo = TRUE,
                           password = password,
                           verbose = verbose)

    xcli_reset_clean = xcli_reset_status == 0L

    if(verbose && xcli_reset_clean) cat("Successfully reset Xcode CLI settings ...\n")

    return(invisible(xcli_reset_clean))
}

#' Interface with `xcode-select` Shell Commands
#'
#' Trigger `xcode-select` commands from within _R_
#'
#' @param args Flag arguments to pass to `xcode-select`
#' @export
#' @rdname xcode-select
xcode_select = function(args) {
    out = sys::exec_internal("xcode-select", args = args, error = FALSE)

    structure(
        list(
            output = sys::as_text(out$stdout),
            status = out$status
        ),
        class = c("xcodeselect", "cli")
    )
}

#' @export
#' @rdname xcode-select
xcode_select_path = function() {
    xcode_select("--print-path")
}

#' @export
#' @rdname xcode-select
xcode_select_version = function() {
    xcode_select("--version")
}
