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

#' @param verbose  Display messages indicating status
#' @param password Password for user account to install software. Default is
#'                 `NULL`.
#' @export
#' @rdname gfortran
gfortran_install = function(verbose = TRUE, password = NULL) {
    assert_mac()
    if(isTRUE(is_gfortran_installed())) {
        if(verbose) {
            message("gfortran is already installed.")
        }
        return(TRUE)
    }

    if (is_macos_r_supported()) {
        if (is_x86_64()) {
            return(install_gfortran_82_mojave())
        } else {
            message("We do not have support for macOS M1/M2 (ARM) architecture yet.")
            return(FALSE)
        }
    } else {
        message("Your version of macOS is not supported.")
        return(FALSE)
    }

    return(TRUE)
}

#' @section Uninstalling `gfortran`:
#' The `gfortran_uninstall()` attempts to remove `gfortran` from
#' two locations:
#'
#' - `/usr/local/gfortran`
#' - `/user/local/bin/gfortran`
#'
#' Using the _R_ sanitized _shell_ command of:
#'
#' ```sh
#' sudo -kS rm -r /usr/local/gfortran /usr/local/bin/gfortran
#' ```
#'
#' These uninstall steps are based on:
#'
#' <https://gcc.gnu.org/wiki/GFortranBinariesMacOS>
#'
#' @export
#' @rdname gfortran
gfortran_uninstall = function(verbose = TRUE, password = NULL) {
    assert_mac()

    if(isFALSE(is_gfortran_installed())) {
        if(verbose) {
            message("gfortran is not installed.")
        }
        return(TRUE)
    }

    shell_execute("rm -r /usr/local/gfortran /usr/local/bin/gfortran",
                  sudo = TRUE,
                  password = password)
}

#' @export
#' @rdname gfortran
gfortran_update = function(verbose = TRUE) {
    assert_mac()
    assert(is_gfortran_installed(), "On gfortran")

}

#' Download and Install gfortran 8.2 for Intel Macs
#'
#' @noRd
install_gfortran_82_mojave = function(password = askpass::askpass(),
                                      verbose = TRUE) {

    # Key the necessary download steps
    gfortran_82_url = "https://mac.r-project.org/tools/gfortran-8.2-Mojave.dmg"
    gfortran_dmg_name = basename(gfortran_82_url)
    gfortran_path = file.path(tempdir(), gfortran_dmg_name)

    # Download the dmg file
    utils::file.download(
        gfortran_82_url,
        gfortran_path
    )

    # Establish where in the dmg the installer package is
    path_to_pkg =
        file.path(
            tools::file_path_sans_ext(gfortran_dmg_name),
            tools::file_path_sans_ext(gfortran_dmg_name),
            "gfortran.pkg"
        )

    # Install the dmg package
    success = install_dmg_package(
        dmg_package_name,
        pkg_location = path_to_pkg,
        password = password,
        verbose = verbose
    )

    if (success) {
        message("Error installing the gfortran package")
        return(FALSE)
    }

    # Remove unused gfortran file
    unlink(gfortran_path)

    return(TRUE)
}


install_dmg_package = function(path_to_dmg,
                               pkg_location,
                               password = NULL,
                               verbose = TRUE) {

    if (verbose) message("Mounting ", volume_name)

    cmd = paste("hdiutil attach", shQuote(path_to_dmg), "-nobrowse -quiet")
    shell_execute(cmd, sudo = FALSE)

    if (verbose) {
        message("Installing ", volume_name, "...")
        message("You may be prompted for your password ...")
    }
    cmd = paste(
        "sudo",
        "-kS",
        "installer",
        "-pkg",
        paste0("'/Volumes/", pkg_location, "'"),
        "-target",
        "/"
    )
    shell_execute(cmd, sudo = TRUE, password = password)

    if (verbose) {
        message("Unmounting ", volume_name, "...")
    }
    cmd = paste("hdiutil", "detach", paste0("'/Volumes/", volume_name, "'"))
    status = shell_execute(cmd, sudo = FALSE)

    status == 0
}
