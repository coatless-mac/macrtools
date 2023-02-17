use_r_environ = function(option, value, path = "~/.Renviron",
                         block_start = "## macrtools: start",
                         block_end = "## macrtools: stop") {

    if (!file.exists(path)) {
        message("`", path ,"` file not found at location.")
        message("Creating a new Renviron file at: ~/.Renviron")
        file.create(path)
    }

    changed = block_append(
        desc = paste0("setting of ", option, "=", value, " into ", path),
        value = paste0(option, "=", value),
        path = path,
        block_start = block_start,
        block_end = block_end
    )

    if (changed) {
        message("Please restart R for the new startup settings to take effect")
    }
    invisible(changed)
}


renviron_gfortran_path = function(path_value = "$PATH:/usr/local/gfortran/bin") {
    # Write the configuration setting into ~/.Renviron
    use_r_environ("PATH", path_value,
                  block_start = "## macrtools - gfortran: start",
                  block_end = "## macrtools - gfortran: end")
}
