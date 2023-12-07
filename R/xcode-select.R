#' Interface with `xcode-select` Shell Commands
#'
#' Trigger `xcode-select` commands from within _R_
#'
#' @param args Flag arguments to pass to `xcode-select`
#' @export
#' @rdname xcode-select
xcode_select = function(args) {
    out = sys::exec_internal("xcode-select", args = args, error = FALSE)

    structure(
        list(
            output = sys::as_text(out$stdout),
            error = sys::as_text(out$stderr),
            status = out$status
        ),
        class = c("xcodeselect", "cli")
    )
}

#' @export
#' @rdname xcode-select
xcode_select_path = function() {
    xcode_select("--print-path")
}

#' @export
#' @rdname xcode-select
xcode_select_version = function() {
    xcode_select("--version")
}
