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

    # For this option, we have to be careful with homebrew installations
    # So, let's aim to check a hard coded path

    # Figure out installation directory
    install_dir = install_location()
    path_gfortran = file.path(install_dir, "gfortran")

    # Check if the directory is present
    dir.exists(path_gfortran)
}

#' @export
#' @rdname gfortran
gfortran_version = function() {
    gfortran("--version")
}

#' @section Install `gfortran`:
#' The `gfortran_install()` function aims to install `gfortran` into
#' two locations:
#'
#' - Intel (`x86_64`)
#'   - `/usr/local/gfortran`
#'   - `/usr/local/bin/gfortran`
#' - M1/M2 (`aarch64`)
#'   - `/opt/r/arm64/gfortran/`
#'   - `/opt/r/arm64/bin/gfortran`
#'
#' The Intel `gfortran` installer is a DMG image that is mounted, installed,
#' and unmounted.
#'
#' The M1/M2 `gfortran` installer is a tar file that is unpacked into the
#' directory.
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

        # Figure out installation directory
        install_dir = install_location()

        # Establish a path
        path_gfortran_bin = file.path(install_dir, "gfortran", "bin")
        path_variable = paste0("${PATH}:", path_gfortran_bin)

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
    install_dir = install_location()

    path_gfortran = file.path(install_dir, "gfortran")
    path_bin_gfortran =file.path(install_dir, "bin", "gfortran")

    shell_execute(
        paste0("rm -rf ", path_gfortran, " ", path_bin_gfortran ),
        sudo = TRUE,
        password = password)
}

#' @section Update `gfortran`:
#' The `gfortran_update()` attempts to update the version of `gfortran` installed
#' using the provided `gfortran-update-sdk` inside of `/opt/R/arm64/gfortran/bin`.
#'
#' Note: This update command only works for M1/M2 (`arm64`) users.
#'
#' @export
#' @rdname gfortran
gfortran_update = function(verbose = TRUE, password = NULL) {
    assert_mac()
    assert_aarch64()
    assert(is_gfortran_installed(), "On gfortran")

    # Figure out installation directory
    install_dir = install_location()

    path_gfortran_update = file.path(install_dir, "gfortran", "bin", "gfortran-update-sdk")

    shell_execute(
        path_gfortran_update,
        sudo = TRUE,
        password = password)
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

    # Download the dmg file
    gfortran_path = binary_download(gfortran_82_url, verbose = verbose)

    # Establish where in the dmg the installer package is
    path_to_pkg =
        file.path(
            tools::file_path_sans_ext(gfortran_dmg_name),
            tools::file_path_sans_ext(gfortran_dmg_name),
            "gfortran.pkg"
        )

    # Install the dmg package
    success = dmg_package_install(
        path_to_dmg = gfortran_path,
        pkg_location_in_dmg = path_to_pkg,
        password = password,
        verbose = verbose
    )

    if (!success) {
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
    install_directory = install_location()
    strip_levels = install_strip_level()

    # Download tar
    path_to_tar = binary_download(gfortran_12_url, verbose = verbose)

    # Install tar into the appropriate location
    tar_package_install(path_to_tar,
                        install_directory,
                        strip_levels,
                        password = password,
                        verbose = verbose)

}
