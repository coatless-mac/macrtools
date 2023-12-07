#' Interface with `xcodebuild` Shell Commands
#'
#' Trigger `xcodebuild` commands from within _R_
#'
#' @param args Flag arguments to pass to `xcodebuild`
#' @export
#' @rdname xcodebuild
xcodebuild = function(args) {
    out = sys::exec_internal("xcodebuild", args = args, error = FALSE)

    structure(
        list(
            output = sys::as_text(out$stdout),
            error = sys::as_text(out$stderr),
            status = out$status
        ),
        class = c("xcodebuild", "cli")
    )
}

#' @export
#' @rdname xcodebuild
xcodebuild_version = function() {
    xcodebuild("-version")
}
