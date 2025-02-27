use_r_environ = function(option, value, path = "~/.Renviron",
                         block_start = "## macrtools: start",
                         block_end = "## macrtools: stop") {

    if (!base::file.exists(path)) {
        base::message("`", path ,"` file not found at location.")
        base::message("Creating a new Renviron file at: ~/.Renviron")
        base::file.create(path)
    }

    changed = block_append(
        desc = base::paste0("setting of ", option, "=", value),
        value = base::paste0(option, "=", value),
        path = path,
        block_start = block_start,
        block_end = block_end
    )

    if (changed) {
        base::message("Please restart R for the new startup settings to take effect")
    }
    base::invisible(changed)
}


renviron_gfortran_path = function(path_value = "$PATH:/usr/local/gfortran/bin") {
    # Write the configuration setting into ~/.Renviron
    use_r_environ("PATH", path_value,
                  block_start = "## macrtools - gfortran: start",
                  block_end = "## macrtools - gfortran: end")
}
