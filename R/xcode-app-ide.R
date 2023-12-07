#' Detect if the Xcode.app IDE is Installed
#'
#' Checks to see whether Xcode.app Integrated Developer Environment
#' (IDE) was installed and is active under the default location.
#'
#' @details
#'
#' Checks to see if Xcode.app IDE is active by running:
#'
#' ```sh
#' xcode-select -p
#' ```
#'
#' If it returns:
#'
#' ```sh
#' /Applications/Xcode.app/Contents/Developer
#' ```
#'
#' We consider the full Xcode.app to be **installed** and **selected** as
#' the command line tools to use.
#'
#' @rdname xcode-app-ide
#' @export
#' @examples
#' # Check if Xcode.app IDE is on the path
#' is_xcode_app_installed()
is_xcode_app_installed = function() {
    assert_mac()

    ## Check to see if the Xcode path is set
    path_info = xcode_select_path()

    identical(path_info$status, 0L) &&
        identical(path_info$output, install_directory_xcode_app()) &&
        dir.exists(path_info$output)

}
