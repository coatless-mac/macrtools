#' @include cli-custom.R
NULL

# Custom CLI Printing -----

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
    cli::cli_text(base::paste0(status_color, x$status, "{.reset}"))

    return(base::invisible(x))
}

# Display download warning ----
verify_status <- function(status, program, url, type = c("uninstall", "install")) {
    type <- base::match.arg(type)

    if(base::isFALSE(status)) {
        cli_error(c(
            "Operation failed: Could not {type} {.pkg {program}}.",
            "Status: {.val {status}}",
            "Operation type: {.val {type}}",
            "Time of failure: {.val {base::format(base::Sys.time(), '%Y-%m-%d %H:%M:%S')}}",
            if(!base::missing(url)) c("Manual instructions available at:", "{.url {url}}")
        ),
        advice = base::paste0("You may need to run this operation with administrative privileges or check for system compatibility issues."))
        return(base::invisible(FALSE))
    }

    base::invisible(TRUE)
}

# Obtain a password if not present ----
force_password <- function(supplied_password) {
    entered_password <- supplied_password
    if(base::is.null(entered_password)) {
        cli_info(c(
            "Administrative privileges required.",
            "Your user account password is needed to execute privileged operations.",
            "This password will not be stored and is only used for the current session.",
            "Current user: {.val {base::Sys.info()['user']}}"
        ))
        entered_password <- askpass::askpass("Please enter your administrator password:")
    }

    entered_password
}

# Get caller environment ----
caller_env <- function(n = 1) {
    base::parent.frame(n + 1)
}

# Helpful null coalesce operator
`%||%` <- function(x, y) {
    if (base::is.null(x)) y else x
}
