#' @include xcode-cli.R gfortran.R recipes.R
NULL

#' Install and Uninstall the macOS R Toolchain
#'
#' The `macos_rtools_install()` function aims to install all required dependencies
#' for the macOS R compilation toolchain. Meanwhile, the `macros_rtools_uninstall()`
#' function aims to remove any installed files from your computer.
#'
#' @param password Password for user account to request `sudo` access.
#' @param verbose  Describe each step taken. Default `TRUE`
#' @details
#'
#' The macOS R compilation toolchain consists of:
#'
#' 1. Xcode CLI
#' 2. gfortran
#' 3. A series of binary packages from the [`recipes`](https://github.com/R-macos/recipes) system to compile R.
#'
#' The `mac_rtools_install()` function attempts to install each of the required
#' components. If we detect that the Xcode.app IDE is installed, we'll skip
#' attempting to install the Xcode CLI software.
#'
#' Meanwhile, the `mac_rtools_uninstall()` function aims to
#' delete or uninstall the Xcode CLI and gfortran binaries. At the present moment,
#' there is no support for uninstalling the binary packages from `recipes`.
#'
#' @section Headless or Unattended Installation:
#'
#' For an installation that does not require a human in the loop, please set
#' the environment variable `macrtools.password` for the user profile:
#'
#' ```r
#' Sys.setenv("macrtools.password" = "your-password-here")
#' ```
#'
#' @rdname macos-rtools
#' @export
#' @examples
#'
#' \dontrun{
#'
#' # Install all tools required for compiling code on macOS with R
#' macos_rtools_install()
#'
#' # Enter a password to avoid being prompted!
#' macos_rtools_install(password = "hello-compiled-code-world")
#'
#' }
macos_rtools_install = function(
    password = getOption("macrtools.password"),
    verbose = TRUE
    ) {

    cat("We're attempting to install Xcode CLI, gfortran, and r-base-dev binaries ... \n")
    cat("Please make sure you are connected to a power source ...\n")
    cat("Please also ensure you have a stable internet connection ...\n")

    cat("\nThis may take some time ... \n")
    cat("Budget for about 15 to 25 minutes ... \n")

    entered_password = password
    if(is.null(entered_password)) {
        cat("Please enter your password in the prompt that is appearing to continue ...\n\n")
        entered_password = askpass::askpass()
    }

    describe_steps = isTRUE(verbose)

    result_xcode = TRUE
    if(!is_xcode_app_installed()) {
        if(!is_xcode_cli_installed()) {
            result_xcode = xcode_cli_install(password = entered_password, verbose = describe_steps)
            if(!result_xcode) stop("Failed to install Xcode CLI. Please see manual steps above.")
        } else {
            if(describe_steps) cat("Xcode CLI was already installed! ...\n")
        }
    } else {
        if(describe_steps) cat("Xcode.app was installed! Skipping the installation for Xcode CLI ...\n")
    }

    result_gfortran = TRUE
    if(!is_gfortran_installed()) {
        result_gfortran = gfortran_install(password = entered_password, verbose = describe_steps)
        if(!result_gfortran) stop("Failed to install gfortran. Please see manual step above.")
    } else {
        if(describe_steps) cat("gfortran was already installed! ...\n")
    }

    result_base_dev = recipes_binary_install(
        "r-base-dev", sudo = TRUE, password = entered_password, verbose = verbose
    )

    rtools_install_clean = result_gfortran && result_xcode && isTRUE(result_base_dev)

    if(rtools_install_clean) {
        cat("\n\nCongratulations! \n")
        cat("Xcode CLI, Gfortran, and R developer binaries have been installed successfully.\n")
    }

    invisible(rtools_install_clean)
}

#' @rdname macos-rtools
#' @export
#' @examples
#'
#' \dontrun{
#'
#' # Remove Xcode CLI and gfortran tools required for compiling code on macOS with R
#' macos_rtools_uninstall()
#'
#' # Enter a password to avoid being prompted!
#' macos_rtools_uninstall(password = "hello-compiled-code-world")
#'
#' }
macos_rtools_uninstall = function(
        password = getOption("macrtools.password"),
        verbose = TRUE
) {

    if(is.null(password)) {
        cat("Please enter your password in the prompt that is appearing to continue ...\n")
        password = askpass::askpass()
    }

    cat("We're attempting to remove both Xcode CLI and gfortran ... \n")
    cat("\nThis may take some time ... \n")
    cat("Please make sure you are connected to a power source ...\n\n")

    result_xcode = TRUE
    if(is_xcode_cli_installed()) {
        result_xcode = xcode_cli_uninstall(password = password, verbose = verbose)
        if(!result_xcode) stop("Failed to uninstall Xcode CLI. Please see manual steps.")
    } else {
        if(verbose) cat("\n\nXcode CLI was not installed, skipping uninstall procedure ...\n\n")
    }

    result_gfortran = TRUE
    if(is_gfortran_installed()) {
        result_gfortran = gfortran_uninstall(password = password, verbose = verbose)
        if(!result_gfortran) stop("Failed to uninstall gfortran. Please see manual steps.")
    } else {
        if(verbose) cat("\n\ngfortran was not installed, skipping uninstall procedure ...\n\n")
    }

    clean = result_gfortran && result_xcode

    if(clean) {
        cat("\n\nCongratulations! \n")
        cat("Xcode CLI and Gfortran binaries have been successfully removed from your system.\n")
    }

    invisible(clean)
}
