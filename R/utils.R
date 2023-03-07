# System functions ----
system_os = function() {
    tolower(Sys.info()[["sysname"]])
}

system_arch = function() {
    R.version$arch
}

# Detect macOS Operating System ----

shell_mac_version = function() {
    sys::as_text(sys::exec_internal("sw_vers", "-productVersion")$stdout)
}


is_macos_r_supported = function() {
    mac_version = shell_mac_version()

    version_between(mac_version, "10.13.0", "14.0.0")
}

is_macos_ventura = function() {
    mac_version = shell_mac_version()

    version_between(mac_version, "13.0.0", "14.0.0")
}

is_macos_monterey = function() {
    mac_version = shell_mac_version()

    version_between(mac_version, "12.0.0", "13.0.0")
}

is_macos_big_sur= function() {
    mac_version = shell_mac_version()

    version_between(mac_version, "11.0.0", "12.0.0")
}

is_macos_catalina = function() {
    mac_version = shell_mac_version()

    version_between(mac_version, "10.15.0", "10.16.0")
}

is_macos_mojave = function() {
    mac_version = shell_mac_version()

    version_between(mac_version, "10.14.0", "10.15.0")
}

is_macos_high_sierra = function() {
    mac_version = shell_mac_version()

    version_between(mac_version, "10.13.0", "10.14.0")
}

is_macos = function() {
    system_os() == "darwin"
}

# Architecture Checks ----

is_aarch64 = function() {
    system_arch() == "aarch64"
}

is_x86_64 = function() {
    system_arch() == "x86_64"
}


# Version Checks ----

is_r_version = function(target_version, compare_major_minor = TRUE) {

    minor_value = if (compare_major_minor) {
        strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1]
    } else {
        R.version$minor
    }

    version_string = paste(R.version$major, minor_value, sep = ".")
    return(version_string == target_version)
}




version_above = function(software_version, than) {
    utils::compareVersion(software_version, than) == 1L
}

version_between = function(software_version, lower, greater_strict) {
    above = utils::compareVersion(software_version, lower) %in% c(0L, 1L)
    below = utils::compareVersion(software_version, greater_strict) %in% c(-1L)
    above && below
}

# Assertions ----

#' Assert a condition
#'
#' @description
#' `assert()` allows a function state to be checked and stopped.
#'
#' @param condition A logical indicating the status of the condition.
#' @param message   A string to display.
#'
#' @keywords internal
#' @rdname assert
#' @export
assert = function(condition, message = NULL) {
    if(isFALSE(condition)) {
        stop(paste0(message, " is not TRUE"))
    }
}

#' @rdname assert
#' @export
assert_mac = function(){
    assert(is_macos(), "On macOS")
}

#' @rdname assert
#' @export
assert_aarch64 = function(){
    assert(is_aarch64(), "On aarch64")
}

#' @rdname assert
#' @export
assert_x86_64 = function(){
    assert(is_x86_64(), "On x86_64")
}

# Custom CLI Printing -----

#' Print CLI Responses
#' @param x   An object with class `cli`
#' @param ... Additional parameters
#' @export
print.cli = function(x, ...) {
    cat("Output:\n", paste(x$output, collapse = "\n"), "\n")
    cat("Status:\n", paste(x$status, collapse = "\n"), "\n")
}

# Display warning ----

verify_status = function(status, program, url, type = c("uninstall", "install")) {
    type = match.arg(type)

    if(isFALSE(status)) {
        cat("We were not able to ", type, program, " ...\n")
        if(!missing(url)) {
            cat("Please try to manually ", type, "using: ..\n")
            cat(url, "\n")
        }
        return(invisible(FALSE))
    }
}


force_password = function(supplied_password) {
    entered_password = supplied_password
    if(is.null(entered_password)) {
        cat("Please enter your password in the prompt that is appearing to continue ...\n\n")
        entered_password = askpass::askpass()
    }

    entered_password
}
