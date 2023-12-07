# Custom CLI Printing -----

#' Print CLI Responses
#' @param x   An object with class `cli`
#' @param ... Additional parameters
#' @export
print.cli = function(x, ...) {

    if(!identical(x$output, character(0)) ) {
        cat("Output:\n", paste(x$output, collapse = "\n"), "\n")
    }

    if(!identical(x$error, character(0)) ) {
        cat("Error:\n", paste(x$error, collapse = "\n"), "\n")
    }

    cat("Status:\n", paste(x$status, collapse = "\n"), "\n")

    return(invisible(x))
}

# Display download warning ----
verify_status = function(status, program, url, type = c("uninstall", "install")) {
    type = match.arg(type)

    if(isFALSE(status)) {
        cat("We were not able to ", type, program, " ...\n")
        if(!missing(url)) {
            cat("Please try to manually ", type, "using: ..\n")
            cat(url, "\n")
        }
        return(invisible(FALSE))
    }
}

# Obtain a password if not present ----
force_password = function(supplied_password) {
    entered_password = supplied_password
    if(is.null(entered_password)) {
        cat("Please enter your password in the prompt that is appearing to continue ...\n\n")
        entered_password = askpass::askpass()
    }

    entered_password
}
