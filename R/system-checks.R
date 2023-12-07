# System functions ----
system_os = function() {
    tolower(Sys.info()[["sysname"]])
}

system_arch = function() {
    R.version$arch
}

# Architecture Checks ----
is_aarch64 = function() {
    system_arch() == "aarch64"
}

is_x86_64 = function() {
    system_arch() == "x86_64"
}
