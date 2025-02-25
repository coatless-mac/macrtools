#' CLI Utility Functions
#'
#' Utility functions for consistent CLI messaging throughout the package.
#'
#' @name cli-utils
#' @keywords internal
NULL

#' Format a message with a package prefix
#'
#' @param msg The message to format
#' @param .envir Environment to evaluate in
#' @return A formatted string with package prefix
#' @keywords internal
format_msg <- function(msg, .envir = parent.frame()) {
    paste0("{.pkg macrtools}: ", cli::format_inline(msg, .envir = .envir))
}

#' Format a path for display
#'
#' @param path The file path to format
#' @return A formatted string with path styling
#' @keywords internal
format_path <- function(path) {
    paste0("{.file ", path, "}")
}

#' Format a command for display
#'
#' @param cmd The command to format
#' @return A formatted string with code styling
#' @keywords internal
format_cmd <- function(cmd) {
    paste0("{.code ", cmd, "}")
}

#' Display an info message
#'
#' @param ... Message parts, passed to cli::format_inline
#' @param .envir Environment to evaluate in
#' @keywords internal
cli_info <- function(..., .envir = parent.frame()) {
    cli::cli_alert_info(format_msg(paste0(...), .envir = .envir))
}

#' Display a success message
#'
#' @param ... Message parts, passed to cli::format_inline
#' @param .envir Environment to evaluate in
#' @keywords internal
cli_success <- function(..., .envir = parent.frame()) {
    cli::cli_alert_success(format_msg(paste0(...), .envir = .envir))
}

#' Display a warning message
#'
#' @param ... Message parts, passed to cli::format_inline
#' @param .envir Environment to evaluate in
#' @keywords internal
cli_warning <- function(..., .envir = parent.frame()) {
    cli::cli_alert_warning(format_msg(paste0(...), .envir = .envir))
}

#' Display a danger/important message
#'
#' @param ... Message parts, passed to cli::format_inline
#' @param .envir Environment to evaluate in
#' @keywords internal
cli_danger <- function(..., .envir = parent.frame()) {
    cli::cli_alert_danger(format_msg(paste0(...), .envir = .envir))
}

#' Throw an error with a formatted message
#'
#' @param ... Message parts, passed to cli::format_inline
#' @param .envir Environment to evaluate in
#' @param call The calling environment
#' @param advice Optional advice to provide to the user
#' @keywords internal
cli_error <- function(..., .envir = parent.frame(), call = caller_env(), advice = NULL) {
    msg <- format_msg(paste0(...), .envir = .envir)
    if (!is.null(advice)) {
        msg <- c(msg, "i" = advice)
    }
    cli::cli_abort(msg, call = call)
}

#' Display a waiting message
#'
#' @param ... Message parts, passed to cli::format_inline
#' @param .envir Environment to evaluate in
#' @keywords internal
cli_waiting <- function(..., .envir = parent.frame()) {
    cli::cli_alert_info(paste0("{.pkg macrtools}: {.file ", paste0(...), "} {.spinner}"))
}

#' Display the start of a process
#'
#' @param msg The process description
#' @param .envir Environment to evaluate in
#' @param total The total number of steps (default: 100)
#' @param clear Whether to clear the progress bar when done (default: FALSE)
#' @return The ID of the progress bar
#' @keywords internal
cli_process_start <- function(msg, .envir = parent.frame(), total = 100, clear = FALSE) {
    cli::cli_progress_bar(
        format = paste0(
            "{.pkg macrtools}: {.strong {.msg {msg}}} {.progress_bar} {.percent {.percent}} {.spinner}"
        ),
        format_done = paste0(
            "{.pkg macrtools}: {.strong {.msg {msg}}} {cli::symbol$tick} Done!"
        ),
        msg = msg,
        total = total,
        clear = clear,
        .envir = .envir
    )
}

#' Update a process progress
#'
#' @param id The progress bar ID
#' @param ratio The progress ratio (0-1) or number of steps completed
#' @param status_msg Optional status message to display
#' @keywords internal
cli_process_update <- function(id, ratio, status_msg = NULL) {
    if (!is.null(status_msg)) {
        cli::cli_progress_update(id = id, set = ratio, msg = status_msg)
    } else {
        cli::cli_progress_update(id = id, set = ratio)
    }
}

#' Complete a process
#'
#' @param id The progress bar ID
#' @param msg Optional completion message
#' @keywords internal
cli_process_done <- function(id, msg = NULL) {
    if (!is.null(msg)) {
        cli::cli_progress_update(id = id, msg = msg)
    }
    cli::cli_progress_done(id = id)
}

#' Display a detailed system update
#'
#' @param title Title of the update section
#' @param items Vector of items to display as a bulleted list
#' @keywords internal
cli_system_update <- function(title, items) {
    cli::cli_h3(title)
    cli::cli_ul(items)
    cli::cli_end()
}
