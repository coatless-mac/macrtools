#' System OS Detection
#'
#' @return The name of the operating system in lowercase
#' @keywords internal
system_os <- function() {
    base::tolower(base::Sys.info()[["sysname"]])
}

#' System Architecture Detection
#'
#' @return The system architecture identifier
#' @keywords internal
system_arch <- function() {
    base::R.version$arch
}

#' Check if System is aarch64
#'
#' @return TRUE if system is Apple Silicon (M-series) Mac, FALSE otherwise
#' @keywords internal
is_aarch64 <- function() {
    system_arch() == "aarch64"
}

#' Check if System is x86_64
#'
#' @return TRUE if system is Intel-based Mac, FALSE otherwise
#' @keywords internal
is_x86_64 <- function() {
    system_arch() == "x86_64"
}

#' Check if System is macOS
#'
#' @return TRUE if system is macOS, FALSE otherwise
#' @keywords internal
is_macos <- function() {
    system_os() == "darwin"
}

#' Get macOS Version
#'
#' @return The macOS version string
#' @keywords internal
shell_mac_version <- function() {
    sys::as_text(sys::exec_internal("sw_vers", "-productVersion")$stdout)
}

#' Check if macOS Version is Supported for R
#'
#' @return TRUE if macOS version is supported, FALSE otherwise
#' @keywords internal
is_macos_r_supported <- function() {
    mac_version <- shell_mac_version()
    version_between(mac_version, "10.13.0", "27.0")
}

#' Check if macOS Tahoe
#'
#' Tahoe is macOS 26.x, released in late 2025.
#'
#' @details
#' macOS Tahoe (version 26.x) is the successor to macOS Sequoia (version 15.x).
#'
#' @return TRUE if system is macOS Tahoe, FALSE otherwise
#' @keywords internal
is_macos_tahoe <- function() {
    mac_version <- shell_mac_version()
    version_between(mac_version, "26.0", "27.0")
}

#' Check if macOS Sequoia
#'
#' Sequoia is macOS 15.x, released in late 2024.
#'
#' @details
#' macOS Sequoia (version 15.x) is the successor to macOS
#' Sonoma (version 14.x).
#'
#' @return TRUE if system is macOS Sequoia, FALSE otherwise
#' @keywords internal
is_macos_sequoia <- function() {
    mac_version <- shell_mac_version()
    version_between(mac_version, "15.0.0", "16.0.0")
}

#' Check if macOS Sonoma
#'
#' Sonoma is macOS 14.x, released in late 2023.
#'
#' @details
#' macOS Sonoma (version 14.x) is the successor to macOS
#' Ventura (version 13.x).
#'
#' @return TRUE if system is macOS Sonoma, FALSE otherwise
#' @keywords internal
is_macos_sonoma <- function() {
    mac_version <- shell_mac_version()
    version_between(mac_version, "14.0.0", "15.0.0")
}

#' Check if macOS Ventura
#'
#' @return TRUE if system is macOS Ventura, FALSE otherwise
#' @keywords internal
is_macos_ventura <- function() {
    mac_version <- shell_mac_version()
    version_between(mac_version, "13.0.0", "14.0.0")
}

#' Check if macOS Monterey
#'
#' @return TRUE if system is macOS Monterey, FALSE otherwise
#' @keywords internal
is_macos_monterey <- function() {
    mac_version <- shell_mac_version()
    version_between(mac_version, "12.0.0", "13.0.0")
}

#' Check if macOS Big Sur
#'
#' @return TRUE if system is macOS Big Sur, FALSE otherwise
#' @keywords internal
is_macos_big_sur <- function() {
    mac_version <- shell_mac_version()
    version_between(mac_version, "11.0.0", "12.0.0")
}

#' Check if macOS Catalina
#'
#' @return TRUE if system is macOS Catalina, FALSE otherwise
#' @keywords internal
is_macos_catalina <- function() {
    mac_version <- shell_mac_version()
    version_between(mac_version, "10.15.0", "10.16.0")
}

#' Check if macOS Mojave
#'
#' @return TRUE if system is macOS Mojave, FALSE otherwise
#' @keywords internal
is_macos_mojave <- function() {
    mac_version <- shell_mac_version()
    version_between(mac_version, "10.14.0", "10.15.0")
}

#' Check if macOS High Sierra
#'
#' @return TRUE if system is macOS High Sierra, FALSE otherwise
#' @keywords internal
is_macos_high_sierra <- function() {
    mac_version <- shell_mac_version()
    version_between(mac_version, "10.13.0", "10.14.0")
}

#' Check if Version is Above Threshold
#'
#' @param software_version Version string to check
#' @param than Threshold version to compare against
#' @return TRUE if software_version is above than, FALSE otherwise
#' @keywords internal
version_above <- function(software_version, than) {
    utils::compareVersion(software_version, than) == 1L
}

#' Check if Version is Between Bounds
#'
#' @param software_version Version string to check
#' @param lower Lower bound for version check (inclusive)
#' @param greater_strict Upper bound for version check (exclusive)
#' @return TRUE if software_version is between bounds, FALSE otherwise
#' @keywords internal
version_between <- function(software_version, lower, greater_strict) {
    above <- utils::compareVersion(software_version, lower) %in% c(0L, 1L)
    below <- utils::compareVersion(software_version, greater_strict) %in% c(-1L)
    above && below
}

#' Check R Version
#'
#' @param target_version Target R version to check against (e.g., "4.0")
#' @param compare_major_minor Whether to compare only major.minor (TRUE) or major.minor.patch (FALSE)
#' @return TRUE if R version matches target_version, FALSE otherwise
#' @keywords internal
is_r_version <- function(target_version, compare_major_minor = TRUE) {
    minor_value <- if (compare_major_minor) {
        # If x.y.z, this retrieves y
        base::strsplit(base::R.version$minor, ".", fixed = TRUE)[[1]][1]
    } else {
        # If x.y.z, this retrieves y.z
        base::R.version$minor
    }

    # Build the version string of x.y or x.y.z
    version_string <- base::paste(base::R.version$major, minor_value, sep = ".")

    # Check for equality.
    return(version_string == target_version)
}
