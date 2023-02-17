#' Find, Install, or Uninstall XCode CLI
#'
#' Set of functions that seek to identify whether XCode CLI was installed,
#' allow XCode CLI to be installed, and removing XCode CLI.
#'
#' @section Check if XCode CLI is installed:
#'
#' Checks using the `xcode-select -p` command to obtain the relevant
#' the directory. If the status code returned is 0, then the tools exist.
#' Otherwise, the return value will be 2 indicating that the tools are missing.
#'
#' Implementation is based on:
#'
#' <https://stackoverflow.com/questions/15371925/how-to-check-if-xcode-command-line-tools-are-installed>
#' @rdname xcode-cli
#' @export
#' @examples
#' # Check if Xcode CLI is installed
#' is_xcode_cli_installed()
is_xcode_cli_installed = function() {
    assert_mac()
    identical(xcode_select_path()$status, 0L)
}

#' @section XCode CLI Installation:
#'
#' Attempts to perform a headless Xcode CLI installation.
#' @export
#' @rdname xcode-cli
#' @param verbose    Display status messages
xcode_cli_install = function(verbose = TRUE, password = NULL){
    assert_mac()
    if(isTRUE(is_xcode_cli_installed())) {
        if(verbose) {
            message("Xcode CLI is already installed.")
        }
        return(TRUE)
    }

    stop("Still a work in progress! Sorry.")

    # Create a temporary in-progress file
    file.create("/tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress")

    # Remove temporary in-progress file
    file.remove("/tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress")
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
#' sudo -kS rm -rf /Library/Developer/CommandLineTools
#' ```
#'
#' If the Xcode application is detect, we note that we did not install the
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
xcode_cli_uninstall = function(verbose = TRUE, password = NULL){
    assert_mac()
    if(isFALSE(is_xcode_cli_installed())) {
        if(verbose) {
            message("Xcode CLI is not installed.")
        }
        return(TRUE)
    }

    if(xcode_cli_path != "/Library/Developer/CommandLineTools") {
        message("We detected the full Xcode application in use at: ", xcode_cli_path)
        message("Please uninstall it using the App store.")
        return(TRUE)
    }

    # Remove the shell execution script
    status = shell_execute("rm -rf /Library/Developer/CommandLineTools",
                  sudo = TRUE,
                  password = password)

    status == 0
}

#' @rdname xcode-cli
#' @export
#' @examples
#' # Check if Xcode CLI is installed
#' xcode_cli_path()
xcode_cli_path = function() {
    xcode_select_path()$output
}



#' Interface with `xcode-select` Shell Commands
#'
#' Trigger `xcode-select` commands from within _R_
#'
#' @param args Flag arguments to pass to `xcode-select`
#' @export
#' @rdname xcode-select
xcode_select = function(args) {
    out = sys::exec_internal("xcode-select", args = args)

    structure(
        list(
            output = sys::as_text(out$stdout),
            status = out$status
        ),
        class = "xcodeselect"
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

#' @param x   An object with class `xcodeselect`
#' @param ... Additional parameters
#' @export
#' @rdname xcode-select
print.xcodeselect = function(x, ...) {
    cat("Output: ", x$output, "\n")
    cat("Status: ", x$status, "\n")
}
