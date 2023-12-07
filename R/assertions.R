# Assertions ----

#' Assert a condition
#'
#' @description
#' `assert()` allows a function state to be checked and stopped.
#'
#' @param condition A logical indicating the status of the condition.
#' @param message   A string to display.
#'
#' @keywords internal
#' @rdname assert
#' @export
assert = function(condition, message = NULL) {
    if(isFALSE(condition)) {
        stop(paste0(message, " is not TRUE"))
    }
}

#' @rdname assert
#' @export
assert_mac = function(){
    assert(is_macos(), "On macOS")
}

#' @rdname assert
#' @export
assert_aarch64 = function(){
    assert(is_aarch64(), "On aarch64")
}

#' @rdname assert
#' @export
assert_x86_64 = function(){
    assert(is_x86_64(), "On x86_64")
}
