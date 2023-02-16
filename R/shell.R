shell_execute = function(cmd, sudo = FALSE, password = NULL) {
    if (sudo) {
        shell_sudo_command(cmd, password)
    } else {
        shell_command(cmd)
    }
}

shell_command = function(cmd) {
    system(cmd)
}

shell_sudo_command = function(cmd, password, prefix = "sudo -kS ") {
    cmd_with_sudo = paste0(prefix, cmd)
    if (is.null(password)) {
        system(cmd_with_sudo, input = password_prompt("Enter user password to run command:"))
    } else {
        system(cmd_with_sudo, input = password)
    }
}

password_prompt = function(msg) {
    if(interactive()) {
        if(requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
            rstudioapi::askForPassword(msg)
        } else {
            readline(msg)
        }
    } else {
        stop("Unable to request password as the session is not being run interactively.")
    }
}
