#' @include shell.R utils.R installers.R renviron.R
NULL


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
    identical(gfortran_version()$status, 0L)
}

#' @export
#' @rdname gfortran
gfortran_version = function() {
    gfortran("--version")
}

#' @section Install `gfortran`:
#'
#'
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

        # Figure out installation directories
        install_dir = install_location()$install_directory

        path_gfortran_bin = file.path(install_dir, "gfortran", "bin")
        path_variable = paste0("$PATH:", path_gfortran_bin)

        status = FALSE
        if (is_x86_64()) {
            status = install_gfortran_82_mojave()
        } else if (is_aarch64()) {
            status = install_gfortran_12_arm()
        } else {
            message("We do not have support for that macOS architecture yet.")
            return( status )
        }

        renviron_gfortran_path(path_variable)

        return( status )
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
#' - Intel (`x86_64`)
#'   - `/usr/local/gfortran`
#'   - `/usr/local/bin/gfortran`
#' - M1/M2 (`aarch64`)
#'   - `/opt/r/arm64/gfortran/`
#'   - `/opt/r/arm64/bin/gfortran`
#'
#' Using the _R_ sanitized _shell_ command of:
#'
#' ```sh
#' sudo -kS rm -r /usr/local/gfortran /usr/local/bin/gfortran
#' ```
#' Or:
#'
#' ```sh
#' sudo -kS rm -r /opt/r/arm64/gfortran/ /opt/r/arm64/bin/gfortran
#' ```
#'
#' These uninstall steps are based on:
#'
#' <https://gcc.gnu.org/wiki/GFortranBinariesMacOS>
#'
#' And the desired path for `aarch64`:
#'
#' <https://mac.r-project.org/tools/>
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

    # Figure out installation directories
    install_dir = install_location()$install_directory

    path_gfortran = file.path(install_dir, "gfortran")
    path_bin_gfortran =file.path(install_dir, "bin", "gfortran")

    shell_execute(
        paste0("rm -rf ", path_gfortran, " ", path_bin_gfortran ),
        sudo = TRUE,
        password = password)
}

#' @export
#' @rdname gfortran
gfortran_update = function(verbose = TRUE, password = NULL) {
    assert_mac()
    assert(is_gfortran_installed(), "On gfortran")

}


gfortran = function(args) {
    out = tryCatch(
        sys::exec_internal("gfortran", args = args, error = FALSE),
        error = function(e) {
            list(
                output = e,
                status = -127L
            )
        }
    )

    structure(
        list(
            output = sys::as_text(out$stdout),
            status = out$status
        ),
        class = c("gfortran", "cli")
    )
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
    utils::download.file(
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
        path_to_dmg = gfortran_path,
        pkg_location_in_dmg = path_to_pkg,
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


install_gfortran_12_arm = function(password = askpass::askpass(),
                                   verbose = TRUE) {

    gfortran_12_url = "https://mac.r-project.org/tools/gfortran-12.0.1-20220312-is-darwin20-arm64.tar.xz"

    # Figure out installation directories
    installation_data = install_location()

    # Download tar
    path_to_tar = download_tar_package(gfortran_12_url)

    # Install tar into the appropriate location
    install_tar_package(path_to_tar,
                        installation_data$install_directory,
                        installation_data$strip_levels,
                        password = password,
                        verbose = verbose)

}
