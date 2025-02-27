#' Interface with `xcode-select` Shell Commands
#'
#' Trigger `xcode-select` commands from within _R_
#'
#' @param args Flag arguments to pass to `xcode-select`
#' @export
#' @rdname xcode-select
xcode_select <- function(args) {
    out <- sys::exec_internal("xcode-select", args = args, error = FALSE)

    base::structure(
        base::list(
            output = sys::as_text(out$stdout),
            error = sys::as_text(out$stderr),
            status = out$status
        ),
        class = c("xcodeselect", "cli")
    )
}

#' @export
#' @rdname xcode-select
xcode_select_path <- function() {
    xcode_select("--print-path")
}

#' @export
#' @rdname xcode-select
xcode_select_version <- function() {
    xcode_select("--version")
}

#' Interface with `xcodebuild` Shell Commands
#'
#' Trigger `xcodebuild` commands from within _R_
#'
#' @param args Flag arguments to pass to `xcodebuild`
#' @export
#' @rdname xcodebuild
xcodebuild <- function(args) {
    out <- sys::exec_internal("xcodebuild", args = args, error = FALSE)

    base::structure(
        base::list(
            output = sys::as_text(out$stdout),
            error = sys::as_text(out$stderr),
            status = out$status
        ),
        class = c("xcodebuild", "cli")
    )
}

#' @export
#' @rdname xcodebuild
xcodebuild_version <- function() {
    xcodebuild("-version")
}

#' Detect if the Xcode.app IDE is Installed
#'
#' Checks to see whether Xcode.app Integrated Developer Environment
#' (IDE) was installed and is active under the default location.
#'
#' @details
#'
#' Checks to see if Xcode.app IDE is active by running:
#'
#' ```sh
#' xcode-select -p
#' ```
#'
#' If it returns:
#'
#' ```sh
#' /Applications/Xcode.app/Contents/Developer
#' ```
#'
#' We consider the full Xcode.app to be **installed** and **selected** as
#' the command line tools to use.
#'
#' @rdname xcode-app-ide
#' @export
#' @examples
#' # Check if Xcode.app IDE is on the path
#' is_xcode_app_installed()
is_xcode_app_installed <- function() {
    assert_mac()

    # Check to see if the Xcode path is set
    path_info <- xcode_select_path()

    base::identical(path_info$status, 0L) &&
        base::identical(path_info$output, install_directory_xcode_app()) &&
        base::dir.exists(path_info$output)
}

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
is_xcode_cli_installed <- function() {
    assert_mac()

    path_info <- xcode_select_path()

    base::identical(path_info$status, 0L) &&
        base::identical(path_info$output, install_directory_xcode_cli()) &&
        base::dir.exists(path_info$output)
}

#' @rdname xcode-cli
#' @export
#' @examples
#' # Determine the path location of Xcode CLI
#' xcode_cli_path()
xcode_cli_path <- function() {
    inquiry_on_path <- xcode_select_path()
    if (base::identical(inquiry_on_path$status, 0L)) {
        inquiry_on_path$output
    } else {
        ""
    }
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
#' @param password   User password to access `sudo`.
xcode_cli_install <- function(password = base::getOption("macrtools.password"), verbose = TRUE){
    assert_mac()

    if (base::isTRUE(is_xcode_cli_installed())) {
        if(verbose) {
            cli::cli_alert_info("{.pkg macrtools}: Xcode CLI is already installed.")
            cli::cli_text("") # Add spacing
        }
        return(base::invisible(TRUE))
    }

    if (base::isTRUE(is_xcode_app_installed())) {
        if(verbose) {
            cli::cli_alert_info("{.pkg macrtools}: Xcode.app IDE is installed.")
            cli::cli_text("Skipping the commandline installation.")
            cli::cli_text("") # Add spacing
        }
        return(base::invisible(TRUE))
    }

    temporary_xcli_file <- "/tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress"

    # Create a temporary in-progress file
    base::file.create(temporary_xcli_file)

    if (verbose) {
        cli::cli_alert_info("{.pkg macrtools}: Checking for available Xcode CLI updates.")
        cli::cli_text("") # Add spacing
    }

    product_information <-
        base::system("softwareupdate -l |
          grep '\\*.*Command Line' |
          tail -n 1 |
          awk -F\"*\" '{print $2}' |
          sed -e 's/^ *//' |
          sed 's/Label: //g' |
          tr -d '\n'", intern = TRUE)

    if (base::length(product_information) == 0) {
        cli::cli_abort(c(
            "{.pkg macrtools}: Could not find Xcode CLI in software updates.",
            "i" = "Try installing manually with 'xcode-select --install' in Terminal."
        ))

        # Remove temporary in-progress file if left in place
        if(base::file.exists(temporary_xcli_file)) {
            base::file.remove(temporary_xcli_file)
        }
        return(base::invisible(FALSE))
    }

    if (verbose) {
        cli::cli_alert_info("{.pkg macrtools}: Installing Xcode CLI.")
        cli::cli_bullets(c(
            "Version: {.val {product_information}}",
            "This process may take 10-15 minutes. Please be patient."
        ))
        cli::cli_text("") # Add spacing
    }

    cmd <- base::paste("softwareupdate", "-i", base::shQuote(product_information), "--verbose")

    xcli_status <- shell_execute(cmd,
                                 sudo = TRUE, password = password, verbose = verbose)

    # Remove temporary in-progress file if left in place
    if(base::file.exists(temporary_xcli_file)) {
        base::file.remove(temporary_xcli_file)
    }

    xcli_clean <- base::identical(xcli_status, 0L)

    if(base::isFALSE(xcli_clean)) {
        cli::cli_abort(c(
            "{.pkg macrtools}: We were not able to install Xcode CLI.",
            "i" = "Please try to manually install using: https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-cli.html#xcode-cli-installation"
        ))
        return(base::invisible(FALSE))
    }

    if (verbose) {
        cli::cli_alert_success("{.pkg macrtools}: Xcode CLI installed successfully!")
        cli::cli_text("") # Add spacing
    }

    return(base::invisible(xcli_clean))
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
xcode_cli_uninstall <- function(password = base::getOption("macrtools.password"), verbose = TRUE){
    assert_mac()

    if(base::isFALSE(is_xcode_cli_installed())) {
        if(verbose) {
            cli::cli_alert_info("{.pkg macrtools}: Xcode CLI is not installed.")
            cli::cli_text("") # Add spacing
        }
        return(base::invisible(TRUE))
    }

    xcli_path <- xcode_cli_path()

    # We should never hit this line of code as the is_xcode_cli_installed() now
    # focuses on only CLI. But, we might want to change that.
    if(xcli_path == install_directory_xcode_app()) {
        cli::cli_alert_warning("{.pkg macrtools}: Full Xcode application detected.")
        cli::cli_bullets(c(
            "Path: {.path {xcode_cli_path()}}",
            "Please uninstall it using the App Store or manually."
        ))
        cli::cli_text("") # Add spacing
        return(base::invisible(TRUE))
    }

    if (verbose) {
        cli::cli_alert_info("{.pkg macrtools}: Uninstalling Xcode CLI.")
        cli::cli_text("") # Add spacing
    }

    # Remove the shell execution script
    xcli_uninstall_status <- shell_execute("rm -rf /Library/Developer/CommandLineTools",
                                           sudo = TRUE,
                                           password = password)

    xcli_uninstall_clean <- base::identical(xcli_uninstall_status, 0L)

    if(base::isFALSE(xcli_uninstall_clean)) {
        cli::cli_abort(c(
            "{.pkg macrtools}: We were not able to uninstall Xcode CLI.",
            "i" = "Please try to manually uninstall using: https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-cli.html#uninstalling-xcode-cli"
        ))
        return(base::invisible(FALSE))
    }

    if (verbose) {
        cli::cli_alert_success("{.pkg macrtools}: Xcode CLI uninstalled successfully!")
        cli::cli_text("") # Add spacing
    }

    base::invisible(xcli_uninstall_clean)
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
xcode_cli_switch <- function(password = base::getOption("macrtools.password"), verbose = TRUE) {
    # The path matches with the default install directory location.
    if (xcode_cli_path() == install_directory_xcode_cli()) {
        if (verbose) {
            cli::cli_alert_info("{.pkg macrtools}: Xcode CLI path is correctly set.")
            cli::cli_text("") # Add spacing
        }
        return(base::invisible(TRUE))
    }

    # Need to modify the location to the current CLI path
    if (verbose) {
        cli::cli_alert_info("{.pkg macrtools}: Setting the Xcode CLI path.")
        cli::cli_bullets(c(
            "Current path: {.path {xcode_cli_path()}}",
            "Target path: {.path {install_directory_xcode_cli()}}"
        ))
        cli::cli_text("") # Add spacing
    }

    cmd <- base::paste("xcode-select", "--switch", install_directory_xcode_cli())

    # Change the directory
    xcli_switch_status <- shell_execute(cmd,
                                        sudo = TRUE,
                                        password = password,
                                        verbose = verbose)

    xcli_switch_clean <- base::identical(xcli_switch_status, 0L)

    if(base::isFALSE(xcli_switch_clean)) {
        cli::cli_abort("{.pkg macrtools}: Failed to switch Xcode CLI path.")
        return(base::invisible(FALSE))
    }

    if (verbose) {
        cli::cli_alert_success("{.pkg macrtools}: Xcode CLI path updated successfully!")
        cli::cli_text("") # Add spacing
    }

    return(base::invisible(xcli_switch_clean))
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
xcode_cli_reset <- function(password = base::getOption("macrtools.password"), verbose = TRUE) {
    if (verbose) {
        cli::cli_alert_info("{.pkg macrtools}: Resetting Xcode CLI to default settings.")
        cli::cli_text("") # Add spacing
    }

    cmd <- base::paste("xcode-select", "--reset")

    # Change the directory
    xcli_reset_status <- shell_execute(cmd,
                                       sudo = TRUE,
                                       password = password,
                                       verbose = verbose)

    xcli_reset_clean <- xcli_reset_status == 0L

    if(base::isFALSE(xcli_reset_clean)) {
        cli::cli_abort("{.pkg macrtools}: Failed to reset Xcode CLI settings.")
        return(base::invisible(FALSE))
    }

    if(verbose) {
        cli::cli_alert_success("{.pkg macrtools}: Successfully reset Xcode CLI settings!")
        cli::cli_text("") # Add spacing
    }

    return(base::invisible(xcli_reset_clean))
}
