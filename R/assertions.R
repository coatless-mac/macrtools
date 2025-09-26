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
    if (base::isFALSE(condition)) {
        cli::cli_abort(c(
            "{.pkg macrtools}: {message}"
        ), call = call)
    }
}

#' @rdname assert
#' @export
assert_mac <- function(call = caller_env()) {
    if (!is_macos()) {
        current_os <- base::tolower(base::Sys.info()[['sysname']])
        cli::cli_abort(c(
            "{.pkg macrtools}: This function requires macOS.",
            "{.pkg macrtools}: The current operating system is {.val {current_os}}."
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
        cli::cli_abort(c(
            "{.pkg macrtools}: Your macOS version {.val {mac_version}} is not supported.",
            "{.pkg macrtools}: Supported versions: macOS High Sierra (10.13) through macOS Tahoe (26.x)."
        ),
        call = call,
        advice = "Please upgrade your macOS to a supported version or use an alternative method to install development tools.")
    }
}

#' @rdname assert
#' @export
assert_aarch64 <- function(call = caller_env()) {
    if (!is_aarch64()) {
        arch <- system_arch()
        cli::cli_abort(c(
            "{.pkg macrtools}: This function requires an Apple Silicon (M-series) Mac.",
            "{.pkg macrtools}: Current architecture: {.val {arch}}."
        ),
        call = call,
        advice = "This feature is specifically designed for Apple Silicon processors (M1, M2, M3, etc.). Intel Macs require different components.")
    }
}

#' @rdname assert
#' @export
assert_x86_64 <- function(call = caller_env()) {
    if (!is_x86_64()) {
        arch <- system_arch()
        cli::cli_abort(c(
            "{.pkg macrtools}: This function requires an Intel-based Mac.",
            "{.pkg macrtools}: Current architecture: {.val {arch}}."
        ),
        call = call,
        advice = "This feature is specifically designed for Intel processors. Apple Silicon Macs require different components.")
    }
}

#' @rdname assert
#' @export
assert_r_version_supported <- function(call = caller_env()) {
    if (!(is_r_version("4.0") || is_r_version("4.1") || is_r_version("4.2") ||
          is_r_version("4.3") || is_r_version("4.4") || is_r_version("4.5"))) {
        version_number <- base::paste(base::R.version$major, base::R.version$minor, sep = ".")
        cli::cli_abort(c(
            "{.pkg macrtools}: The installed R version {.val {version_number}} is not supported.",
            "{.pkg macrtools}: Supported versions: R 4.0.x through R 4.5.x."
        ),
        call = call,
        advice = "Please upgrade or downgrade your R installation to a supported version.")
    }
}
