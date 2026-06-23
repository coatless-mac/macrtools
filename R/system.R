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

#' Capture the Text Output of an External Command
#'
#' Runs `command` with `args` via [sys::exec_internal()] and returns its
#' standard output as trimmed text. If the command cannot be run, `fallback`
#' is returned instead.
#'
#' @param command  Name of the program to execute.
#' @param args     Character vector of arguments passed to the program.
#' @param fallback Value returned when execution fails. Default `"Unknown"`.
#' @return The command's standard output as text, or `fallback` on error.
#' @keywords internal
exec_text <- function(command, args, fallback = "Unknown") {
    base::tryCatch(
        sys::as_text(sys::exec_internal(command, args)$stdout),
        error = function(e) fallback
    )
}

#' Check if the macOS Version Falls in a Range
#'
#' @param lower Lower bound for the macOS version (inclusive).
#' @param upper Upper bound for the macOS version (exclusive).
#' @return TRUE if the running macOS version is in `[lower, upper)`.
#' @keywords internal
macos_version_in_range <- function(lower, upper) {
    version_between(shell_mac_version(), lower, upper)
}

#' Check if macOS Version is Supported for R
#'
#' @return TRUE if macOS version is supported, FALSE otherwise
#' @keywords internal
is_macos_r_supported <- function() {
    macos_version_in_range("10.13.0", "27.0")
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
    macos_version_in_range("26.0", "27.0")
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
    macos_version_in_range("15.0.0", "16.0.0")
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
    macos_version_in_range("14.0.0", "15.0.0")
}

#' Check if macOS Ventura
#'
#' @return TRUE if system is macOS Ventura, FALSE otherwise
#' @keywords internal
is_macos_ventura <- function() {
    macos_version_in_range("13.0.0", "14.0.0")
}

#' Check if macOS Monterey
#'
#' @return TRUE if system is macOS Monterey, FALSE otherwise
#' @keywords internal
is_macos_monterey <- function() {
    macos_version_in_range("12.0.0", "13.0.0")
}

#' Check if macOS Big Sur
#'
#' @return TRUE if system is macOS Big Sur, FALSE otherwise
#' @keywords internal
is_macos_big_sur <- function() {
    macos_version_in_range("11.0.0", "12.0.0")
}

#' Check if macOS Catalina
#'
#' @return TRUE if system is macOS Catalina, FALSE otherwise
#' @keywords internal
is_macos_catalina <- function() {
    macos_version_in_range("10.15.0", "10.16.0")
}

#' Check if macOS Mojave
#'
#' @return TRUE if system is macOS Mojave, FALSE otherwise
#' @keywords internal
is_macos_mojave <- function() {
    macos_version_in_range("10.14.0", "10.15.0")
}

#' Check if macOS High Sierra
#'
#' @return TRUE if system is macOS High Sierra, FALSE otherwise
#' @keywords internal
is_macos_high_sierra <- function() {
    macos_version_in_range("10.13.0", "10.14.0")
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

#' Running R Version as a Comparable major.minor String
#'
#' @return The running R version as a `"major.minor"` string, e.g. `"4.6"`.
#' @keywords internal
r_version_major_minor <- function() {
    # If R.version$minor is "y.z", this retrieves just "y"
    minor_value <- base::strsplit(base::R.version$minor, ".", fixed = TRUE)[[1]][1]
    base::paste(base::R.version$major, minor_value, sep = ".")
}

#' Running R Version as a Full major.minor.patch String
#'
#' @return The running R version as a `"major.minor.patch"` string, e.g. `"4.6.0"`.
#' @keywords internal
r_version_full <- function() {
    base::paste(base::R.version$major, base::R.version$minor, sep = ".")
}

#' Supported R Version Window
#'
#' Single source of truth for the oldest and newest R minor versions whose
#' macOS toolchain `macrtools` supports.
#'
#' @details
#' To validate support for a new R release, bump
#' [maximum_supported_r_version()]. A new toolchain *branch* (e.g. in
#' [gfortran_install()] or the `installers.R` helpers) is only required when
#' the toolchain itself changes for that release; the range checks
#' ([is_r_version_at_least()]) otherwise carry the newest tier forward
#' automatically.
#'
#' @return A `"major.minor"` version string.
#' @keywords internal
#' @name supported_r_version
minimum_supported_r_version <- function() "4.0"

#' @rdname supported_r_version
#' @keywords internal
maximum_supported_r_version <- function() "4.6"

#' Check if the Running R Version is At Least a Target
#'
#' @param target  Target R version (e.g. `"4.3"`) to compare against.
#' @param version The version to test, defaults to the running R version.
#' @return TRUE if `version` is greater than or equal to `target`, FALSE otherwise
#' @keywords internal
is_r_version_at_least <- function(target, version = r_version_major_minor()) {
    utils::compareVersion(version, target) >= 0L
}

#' Check if the Running R Version is Supported
#'
#' @param version The version to test, defaults to the running R version.
#' @return TRUE if `version` falls within the supported window (inclusive),
#'   FALSE otherwise
#' @keywords internal
is_r_version_supported <- function(version = r_version_major_minor()) {
    utils::compareVersion(version, minimum_supported_r_version()) >= 0L &&
        utils::compareVersion(version, maximum_supported_r_version()) <= 0L
}

#' Check R Version
#'
#' @param target_version Target R version to check against (e.g., "4.0")
#' @param compare_major_minor Whether to compare only major.minor (TRUE) or major.minor.patch (FALSE)
#' @return TRUE if R version matches target_version, FALSE otherwise
#' @keywords internal
is_r_version <- function(target_version, compare_major_minor = TRUE) {
    version_string <- if (compare_major_minor) {
        # If x.y.z, this compares against x.y
        r_version_major_minor()
    } else {
        # If x.y.z, this compares against the full x.y.z
        r_version_full()
    }

    # Check for equality.
    return(version_string == target_version)
}
