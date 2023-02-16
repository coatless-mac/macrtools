shell_execute = function(cmd, sudo = FALSE) {
    if (sudo) {
        shell_sudo_command(cmd)
    } else {
        shell_command(cmd)
    }
}

shell_command = function(cmd) {
    system(cmd)
}

shell_sudo_command = function(cmd, prefix = "sudo -kS ") {
    cmd_with_sudo = paste0(prefix, cmd)
    system(cmd_with_sudo, input = password_prompt("Enter user password to run command:"))
}

password_prompt = function(msg) {
    if(interactive()) {
        if(rstudioapi::isAvailable()) {
            rstudioapi::askForPassword(msg)
        } else {
            readline(msg)
        }
    } else {
        stop("Unable to request password as the session is not being run interactively.")
    }
}
