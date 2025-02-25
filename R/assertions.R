#' @include cli-custom.R


#' @title Assert a condition
#'
#' @description
#' `assert()` checks a condition and throws an error with a formatted message if it fails.
#'
#' @param condition A logical indicating the status of the condition.
#' @param message   A string to display in the error message.
#' @param call      The calling environment (default: caller_env()).
#'
#' @keywords internal
#' @rdname assert
#' @export
assert <- function(condition, message = NULL, call = caller_env()) {
    if (isFALSE(condition)) {
        cli_error(message, call = call)
    }
}

#' @rdname assert
#' @export
assert_mac <- function(call = caller_env()) {
    if (!is_macos()) {
        cli_error(c(
            "This function requires macOS.",
            "The current operating system is {.val {tolower(Sys.info()[['sysname']])}}."
        ),
        call = call,
        advice = "macrtools only works on macOS systems with Intel or Apple Silicon processors.")
    }
}

#' @rdname assert
#' @export
assert_macos_supported <- function(call = caller_env()) {
    assert_mac(call = call)

    if (!is_macos_r_supported()) {
        mac_version <- shell_mac_version()
        cli_error(c(
            "Your macOS version {.val {mac_version}} is not supported.",
            "Supported versions: macOS High Sierra (10.13) through macOS Sequoia (15.x)."
        ),
        call = call,
        advice = "Please upgrade your macOS to a supported version or use an alternative method to install development tools.")
    }
}

#' @rdname assert
#' @export
assert_aarch64 <- function(call = caller_env()) {
    if (!is_aarch64()) {
        cli_error(c(
            "This function requires an Apple Silicon (M-series) Mac.",
            "Current architecture: {.val {system_arch()}}."
        ),
        call = call,
        advice = "This feature is specifically designed for Apple Silicon processors (M1, M2, M3, etc.). Intel Macs require different components.")
    }
}

#' @rdname assert
#' @export
assert_x86_64 <- function(call = caller_env()) {
    if (!is_x86_64()) {
        cli_error(c(
            "This function requires an Intel-based Mac.",
            "Current architecture: {.val {system_arch()}}."
        ),
        call = call,
        advice = "This feature is specifically designed for Intel processors. Apple Silicon Macs require different components.")
    }
}

#' @rdname assert
#' @export
assert_r_version_supported <- function(call = caller_env()) {
    if (!(is_r_version("4.0") || is_r_version("4.1") || is_r_version("4.2") ||
          is_r_version("4.3") || is_r_version("4.4"))) {
        version_number <- paste(R.version$major, R.version$minor, sep = ".")
        cli_error(c(
            "The installed R version {.val {version_number}} is not supported.",
            "Supported versions: R 4.0.x through R 4.4.x."
        ),
        call = call,
        advice = "Please upgrade or downgrade your R installation to a supported version.")
    }
}
