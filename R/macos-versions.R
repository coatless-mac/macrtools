# Detect macOS Operating System ----

shell_mac_version = function() {
    sys::as_text(sys::exec_internal("sw_vers", "-productVersion")$stdout)
}

is_macos_r_supported = function() {
    mac_version = shell_mac_version()

    version_between(mac_version, "10.13.0", "16.0.0")
}

is_macos_sequoia = function() {
    mac_version = shell_mac_version()

    version_between(mac_version, "15.0.0", "16.0.0")
}

is_macos_sonoma = function() {
    mac_version = shell_mac_version()

    version_between(mac_version, "14.0.0", "15.0.0")
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
