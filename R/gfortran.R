#' Find, Install, or Uninstall gfortran
#'
#' Set of functions that seek to identify whether gfortran was installed,
#' allow gfortran to be installed, and removing gfortran.
#'
#' @section Check if `gfortran` is installed:
#'
#' Checks using the `gfortran --version` command to see if a response is present.
#'
#' @rdname gfortran
#' @export
#' @examples
#' # Check if gfortran is installed
#' is_gfortran_installed()
is_gfortran_installed = function() {
    assert_mac()
    identical(xcode_select_path()$status, 0L)
}

#' @export
#' @rdname gfortran
gfortran_install = function(verbose = TRUE) {
    assert_mac()
    if(isTRUE(is_gfortran_installed())) {
        if(verbose) {
            message("gfortran is already installed.")
        }
        return(TRUE)
    }
}

#' @export
#' @rdname gfortran
gfortran_uninstall = function(verbose = TRUE) {
    assert_mac()

    if(isFALSE(is_gfortran_installed())) {
        if(verbose) {
            message("gfortran is not installed.")
        }
        return(TRUE)
    }
}

#' @export
#' @rdname gfortran
gfortran_update = function(verbose = TRUE) {
    assert_mac()
    assert(is_gfortran_installed(), "On gfortran")

}
