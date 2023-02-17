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
        system(cmd_with_sudo, input = askpass::askpass("Please enter your password:"))
    } else {
        system(cmd_with_sudo, input = password)
    }
}
