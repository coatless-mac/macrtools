#' Installation Directory Locations
#'
#' Pre-defined constants for each software install location.
#'
#' @return
#' A string containing a fixed path to the installation directory.
#'
#' @details
#' We have the following fixed paths:
#'
#' - Intel (x86_64)
#'   - `/usr/local`
#' - M1 or M2 (arm64 or aarch64)
#'   - `/opt/R/arm64`
#' - Xcode CLI
#'   - `/Library/Developer/CommandLineTools`
#' - Xcode App
#'   - `/Applications/Xcode.app/Contents/Developer`
#'
#' @export
#' @rdname install-directory
install_directory_x86_64 = function() {
    "/usr/local"
}

#' @export
#' @rdname install-directory
install_directory_arm64 = function() {
    "/opt/R/arm64"
}

#' @export
#' @rdname install-directory
install_directory_xcode_cli = function() {
    "/Library/Developer/CommandLineTools"
}

#' @export
#' @rdname install-directory
install_directory_xcode_app = function() {
    "/Applications/Xcode.app/Contents/Developer"
}