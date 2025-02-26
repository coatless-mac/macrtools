#' Print CLI Responses
#' @param x   An object with class `cli`
#' @param ... Additional parameters
#' @export
print.cli <- function(x, ...) {
    if(!base::identical(x$output, character(0))) {
        cli::cli_h1("Output")
        cli::cli_text(base::paste(x$output, collapse = "\n"))
    }

    if(!base::identical(x$error, character(0))) {
        cli::cli_h1("Error")
        cli::cli_alert_danger(base::paste(x$error, collapse = "\n"))
    }

    cli::cli_h1("Status")
    status_color <- if(x$status == 0) "{.green}" else "{.red}"
    cli::cli_text("{status_color}{x$status}{.reset}")

    return(base::invisible(x))
}

#' Verify Status of Operation
#'
#' @param status Status code from operation
#' @param program Name of the program being installed or uninstalled
#' @param url Optional URL for manual instructions
#' @param type Type of operation ("uninstall" or "install")
#' @return TRUE if status is successful, FALSE otherwise (invisibly)
#' @keywords internal
verify_status <- function(status, program, url, type = c("uninstall", "install")) {
    type <- base::match.arg(type)

    if(base::isFALSE(status)) {
        time_of_failure <- base::format(base::Sys.time(), '%Y-%m-%d %H:%M:%S')
        url_info <- if(!base::missing(url)) c("Manual instructions available at:", "{.url {url}}") else NULL

        cli::cli_abort(c(
            "{.pkg macrtools}: Operation failed: Could not {type} {.pkg {program}}.",
            "{.pkg macrtools}: Status: {.val {status}}",
            "{.pkg macrtools}: Operation type: {.val {type}}",
            "{.pkg macrtools}: Time of failure: {.val {time_of_failure}}",
            url_info
        ),
        advice = base::paste0("You may need to run this operation with administrative privileges or check for system compatibility issues."))
        return(base::invisible(FALSE))
    }

    base::invisible(TRUE)
}

#' Force Password Entry if Not Provided
#'
#' @param supplied_password Password provided by user (may be NULL)
#' @return Password to use for operations
#' @keywords internal
force_password <- function(supplied_password) {
    entered_password <- supplied_password
    if(base::is.null(entered_password)) {
        current_user <- base::Sys.info()['user']

        cli::cli_alert_info(c(
            "{.pkg macrtools}: Administrative privileges required.",
            "Your user account password is needed to execute privileged operations.",
            "This password will not be stored and is only used for the current session.",
            "Current user: {.val {current_user}}"
        ))
        entered_password <- askpass::askpass("Please enter your administrator password:")
    }

    entered_password
}

#' Get Caller Environment
#'
#' @param n Number of frames to go back
#' @return Caller environment
#' @keywords internal
caller_env <- function(n = 1) {
    base::parent.frame(n + 1)
}

#' Null Coalesce Operator
#'
#' @param x First value (may be NULL)
#' @param y Default value if x is NULL
#' @return x if not NULL, otherwise y
#' @name null_coalesce
#' @keywords internal
`%||%` <- function(x, y) {
    if (base::is.null(x)) y else x
}
