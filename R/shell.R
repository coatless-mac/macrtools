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
        working_dir <- base::getwd()
        cli::cli_alert_info("{.pkg macrtools}: Executing shell command.")
        cli::cli_bullets(c(
            "Command: {.code {cmd}}",
            "Working directory: {.path {working_dir}}"
        ))
        cli::cli_text("") # Add spacing
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
    cmd_with_sudo <- base::paste0(prefix, cmd)
    if (verbose) {
        cli::cli_alert_info("{.pkg macrtools}: Executing privileged command with sudo.")
        cli::cli_bullets(c(
            "Command: {.code {cmd_with_sudo}}",
            "Administrative privileges will be required."
        ))
        cli::cli_text("") # Add spacing
    }

    result <- if (base::is.null(password)) {
        base::system(cmd_with_sudo, input = askpass::askpass("Please enter your administrator password:"))
    } else {
        base::system(cmd_with_sudo, input = password)
    }

    if (verbose && result != 0) {
        cli::cli_alert_warning(c(
            "{.pkg macrtools}: Command execution failed.",
            "Status code: {.val {result}}",
            "This might indicate permission issues or syntax errors."
        ))
        cli::cli_text("") # Add spacing
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
    command_start_time <- base::Sys.time()

    if (verbose) {
        # Use temporary variable for the sudo prefix text
        sudo_prefix <- if (sudo) "sudo " else ""
        cli::cli_alert_info("{.pkg macrtools}: Executing command.")
        cli::cli_code("{sudo_prefix}{cmd}")
        cli::cli_text("") # Add spacing
    }

    result <- if (sudo) {
        shell_sudo_command(cmd = cmd, password = password, verbose = FALSE)
    } else {
        shell_command(cmd = cmd, verbose = FALSE)
    }

    command_duration <- base::difftime(base::Sys.time(), command_start_time, units = "secs")
    duration_seconds <- base::round(base::as.numeric(command_duration), 2)

    if (verbose) {
        if (result == 0) {
            cli::cli_alert_success("{.pkg macrtools}: Command completed successfully.")
            cli::cli_bullets(c(
                "Execution time: {.val {duration_seconds}} seconds",
                "Exit status: {.val {result}}"
            ))
            cli::cli_text("") # Add spacing
        } else {
            cli::cli_alert_warning("{.pkg macrtools}: Command completed with non-zero exit status.")
            cli::cli_bullets(c(
                "Execution time: {.val {duration_seconds}} seconds",
                "Exit status: {.val {result}}"
            ))
            cli::cli_text("") # Add spacing
        }
    }

    return(result)
}
