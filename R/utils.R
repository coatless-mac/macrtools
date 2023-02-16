system_os = function() {
    tolower(Sys.info()[["sysname"]])
}

system_arch = function() {
    R.version$arch
}

shell_mac_version = function() {
    sys::as_text(sys::exec_internal("sw_vers", "-productVersion")$stdout)
}

is_macos_ventura = function() {
    mac_version = shell_mac_version()

    version_between(mac_version, "13.0.0", "14.0.0")
}


is_macos = function() {
    system_os() == "mac"
}

is_aarch64 = function() {
    system_arch() == "aarch64"
}

is_x86_64 = function() {
    system_arch() == "x86_64"
}

version_above = function(software_version, than) {
    utils::compareVersion(software_version, than) == 1L
}

version_between = function(software_version, lower, greater_strict) {
    above = utils::compareVersion(software_version, lower) %in% c(0L, 1L)
    below = utils::compareVersion(software_version, greater_strict) %in% c(-1L)
    above && below
}


#' Assert a condition
#'
#' @description
#' `assert()` allows a function state to be checked and stopped.
#'
#' @param condition A logical indicating the status of the condition.
#' @param message   A string to display.
#'
#' @keywords internal
#' @noRd
assert = function(condition, message = NULL) {
    if(isFALSE(condition)) {
        stop(paste0(message, "is not TRUE"))
    }
}

assert_mac = function(){
    assert(is_macos(), "On macOS")
}


assert_aarch64 = function(){
    assert(is_aarch64(), "On aarch64")
}

assert_x86_64 = function(){
    assert(is_x86_64(), "On x86_64")
}

