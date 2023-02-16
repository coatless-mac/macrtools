#' Find, Install, or Uninstall XCode CLI
#'
#' Set of functions that seek to identify whether XCode CLI was installed,
#' allow XCode CLI to be installed, and removing XCode CLI.
#'
#' @section XCode CLI Installation Check:
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
is_xcode_cli_installed = function() {
    assert_mac()
    identical(xcode_select_path()$status_code, 0L)
}

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

xcode_select_path = function() {
    xcode_select("--print-path")
}

xcode_select_version = function() {
    xcode_select("--version")
}

xcode_select = function(args) {
    out = sys::exec_internal("xcode-select", args = args)
    status_code = sys::exec_status(pid)

    structure(
        list(
            output = sys::as_text(out$stdout),
            status = status_code
        ),
        class = "xcodeselect"
    )
}
