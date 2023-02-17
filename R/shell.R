shell_command = function(cmd, verbose = TRUE) {
    if (verbose) {
        cat("Running command without sudo ...\n")
        cat(cmd, "\n")
    }
    system(cmd)
}

shell_sudo_command = function(cmd, password, prefix = "sudo -kS ", verbose = TRUE) {
    cmd_with_sudo = paste0(prefix, cmd)
    if (verbose) {
        cat("Running command with sudo ...\n")
        cat(cmd_with_sudo, "\n")
    }
    if (is.null(password)) {
        system(cmd_with_sudo, input = askpass::askpass("Please enter your password:"))
    } else {
        system(cmd_with_sudo, input = password)
    }
}

shell_execute = function(cmd, sudo = FALSE, password = NULL, verbose = TRUE) {
    if (sudo) {
        shell_sudo_command(cmd, password, verbose)
    } else {
        shell_command(cmd, verbose)
    }
}
