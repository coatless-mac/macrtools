# Version Checks ----

version_above = function(software_version, than) {
    utils::compareVersion(software_version, than) == 1L
}

version_between = function(software_version, lower, greater_strict) {
    above = utils::compareVersion(software_version, lower) %in% c(0L, 1L)
    below = utils::compareVersion(software_version, greater_strict) %in% c(-1L)
    above && below
}


is_r_version = function(target_version, compare_major_minor = TRUE) {

    minor_value = if (compare_major_minor) {
        # If x.y.z, this retrieves y
        strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1]
    } else {
        # If x.y.z, this retrieves y.z
        R.version$minor
    }

    # Build the version string of x.y or x.y.z
    version_string = paste(R.version$major, minor_value, sep = ".")

    # Check for equality.
    return(version_string == target_version)
}

