#' @include cli-custom.R
NULL

#' Execute shell commands
#'
#' Functions to execute shell commands with or without sudo
#'
#' @param cmd The command to execute
#' @param verbose Display the command being executed
#' @return The exit status of the command (0 for success)
#' @keywords internal
shell_command <- function(cmd, verbose = TRUE) {
    if (verbose) {
        cli_info(c(
            "Executing shell command.",
            "Command: {.code {cmd}}",
            "Working directory: {.path {getwd()}}"
        ))
    }
    base::system(cmd)
}

#' Execute shell command with sudo
#'
#' @param cmd The command to execute
#' @param password User password for sudo privileges
#' @param verbose Display the command being executed
#' @param prefix The sudo prefix (default: "sudo -kS ")
#' @return The exit status of the command (0 for success)
#' @keywords internal
shell_sudo_command <- function(cmd, password, verbose = TRUE, prefix = "sudo -kS ") {
    cmd_with_sudo <- paste0(prefix, cmd)
    if (verbose) {
        cli_info(c(
            "Executing privileged command with sudo.",
            "Command: {.code {cmd_with_sudo}}",
            "Administrative privileges will be required."
        ))
    }

    if (is.null(password)) {
        result <- base::system(cmd_with_sudo, input = askpass::askpass("Please enter your administrator password:"))
    } else {
        result <- base::system(cmd_with_sudo, input = password)
    }

    if (verbose && result != 0) {
        cli_warning(c(
            "Command execution failed with status: {.val {result}}.",
            "This might indicate permission issues or syntax errors."
        ))
    }

    return(result)
}

#' Execute a shell command
#'
#' Execute a command with or without sudo privileges
#'
#' @param cmd The command to execute
#' @param sudo Whether to use sudo (default: FALSE)
#' @param password User password for sudo (only required when sudo=TRUE)
#' @param verbose Display the command being executed
#' @param timeout Timeout in seconds (default: 300)
#' @return The exit status of the command (0 for success)
#' @keywords internal
shell_execute <- function(cmd, sudo = FALSE, password = NULL, verbose = TRUE, timeout = 300) {
    command_start_time <- Sys.time()

    result <- if (sudo) {
        shell_sudo_command(cmd = cmd, password = password, verbose = verbose)
    } else {
        shell_command(cmd = cmd, verbose = verbose)
    }

    command_duration <- difftime(Sys.time(), command_start_time, units = "secs")

    if (verbose) {
        if (result == 0) {
            cli_info(c(
                "Command completed successfully in {.val {round(as.numeric(command_duration), 2)}} seconds.",
                "Exit status: {.val {result}}"
            ))
        } else {
            cli_warning(c(
                "Command completed with non-zero exit status in {.val {round(as.numeric(command_duration), 2)}} seconds.",
                "Exit status: {.val {result}}"
            ))
        }
    }

    return(result)
}
