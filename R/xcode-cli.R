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
#' Checks using the `xcode-select -p` command to obtain the relevant
#' the directory. If the status code returned is 0, then the tools exist.
#' Otherwise, the return value will be 2 indicating that the tools are missing.
#' @export
#' @rdname xcode-cli
#' @param verbose    Display status messages
xcode_cli_install = function(verbose = TRUE){
    assert_mac()
    if(isTRUE(is_xcode_cli_installed())) {
        if(verbose) {
            message("Xcode CLI is already installed.")
        }
        return(TRUE)
    }

    # Create a temporary in-progress file
    file.create("/tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress")

    # Remove temporary in-progress file
    file.remove("/tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress")
}



#' @export
#' @rdname xcode-cli
#' @param verbose    Display status messages
xcode_cli_uninstall = function(verbose = TRUE){
    assert_mac()
    if(isFALSE(is_xcode_cli_installed())) {
        if(verbose) {
            message("Xcode CLI is not installed.")
        }
        return(TRUE)
    }

    # file.unlink("")
}

#' @rdname xcode-cli
#' @export
#' @examples
#' # Check if Xcode CLI is installed
#' xcode_cli_path()
xcode_cli_path = function() {
    xcode_select_path()
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
