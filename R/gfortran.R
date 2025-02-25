#' @include shell.R utils.R installers.R renviron.R cli-custom.R
NULL


#' Find, Install, or Uninstall gfortran
#'
#' Set of functions that seek to identify whether gfortran was installed,
#' allow gfortran to be installed, and removing gfortran.
#'
#' @details
#' The `gfortran` suite of functions attempts to locate, install, and uninstall
#' `gfortran` based the default installation locations that depend on architecture:
#'
#' - Intel (`x86_64`) and M-series (`arm64` or `aarch64`) for R >= 4.3
#'   - `/opt/gfortran/`
#'   - `/opt/gfortran/bin`
#' - Intel (`x86_64`) for R < 4.3
#'   - `/usr/local/gfortran`
#'   - `/usr/local/bin/gfortran`
#' - M-series (`arm64` or `aarch64`) for 4.1 <= R < 4.3
#'   - `/opt/R/arm64/gfortran/`
#'   - `/opt/R/arm64/bin/gfortran`
#'
#' @section Check if `gfortran` is installed:
#'
#' Checks the local file system for whether `gfortran` is installed in
#' the default installation location.
#'
#' @rdname gfortran
#' @export
#' @examples
#' # Check if gfortran is installed
#' is_gfortran_installed()
is_gfortran_installed <- function() {
    assert_mac()

    # For this option, we have to be careful with homebrew installations
    # So, let's aim to check a hard coded path

    # Figure out installation directory
    install_dir <- gfortran_install_location()
    path_gfortran <- file.path(install_dir, "gfortran")

    # Check if the directory is present
    dir.exists(path_gfortran)
}

#' @export
#' @rdname gfortran
gfortran_version <- function() {
    gfortran("--version")
}

#' @section Installing `gfortran`:
#' The `gfortran_install()` function aims to install `gfortran` into the
#' appropriate location for **Intel** (`x86_64`) or **M-series** (`arm64`/`aarch64`)
#' depending on the R version used.
#'
#' ### gfortran Installation for R 4.3
#'
#' The `gfortran` installer for R 4.3 is a universal installer that places
#' `gfortran` into the `/opt/gfortran` for both **Intel** (`x86_64`) and
#' **M-series** (`arm64`/`aarch64`) macs.
#'
#' ```sh
#' # Install the downloaded package into root
#' sudo installer \
#'   -pkg /path/to/gfortran-12.2-universal.pkg \
#'   -target /
#' ```
#'
#' Once installed, we modify the `PATH` environment variable to recognize the newly
#' installed software by adding into the `~/.Renviron` file the following:
#'
#' ```sh
#' touch ~/.Renviron
#' cat << "EOF" > ~./Renviron
#' ## macrtools - gfortran: start
#' PATH=${PATH}:/opt/gfortran/bin
#' ## macrtools - gfortran: end
#' EOF
#' ```
#'
#' ### gfortran Installation for Intel Macs (`x86_64`)
#'
#' The Intel `gfortran` installer is a DMG image that is mounted, installed,
#' and unmounted. We're using the following set of _R_ sanitized _shell_ commands:
#'
#' ```sh
#' # Mount the `.dmg` installer image
#' hdiutil attach "$path_to_dmg" -nobrowse -quiet
#'
#' # Install the package from DMG image into root
#' sudo installer \
#'   -pkg /Volume/gfortran-8.2-Mojave/gfortran-8.2-Mojave/gfortran.pkg \
#'   -target /
#'
#' # Unmount the `.dmg` installer image
#' hdiutil detach /Volumes/gfortran-8.2-Mojave
#' ```
#'
#' Lastly, we modify the `PATH` environment variable to recognize the newly
#' installed software by adding into the `~/.Renviron` file the following:
#'
#' ```sh
#' touch ~/.Renviron
#' cat << "EOF" > ~./Renviron
#' ## macrtools - gfortran: start
#' PATH=${PATH}:/usr/local/gfortran/bin
#' ## macrtools - gfortran: end
#' EOF
#' ```
#'
#' ### gfortran Installation for M-series Macs (`arm64`)
#'
#' The **M-series** `gfortran` installer is a tar file that is unpacked into the
#' directory. Depending on the _R_ version, we opt to install either
#' **gfortran 12 for R 4.2** or **gfortran 11 for R 4.1**
#'
#' If users are on **R 4.2** with an **M-series** mac, then the _R_
#' sanitized _shell_ commands used are:
#'
#' ```sh
#' URL="https://mac.r-project.org/tools/gfortran-12.0.1-20220312-is-darwin20-arm64.tar.xz"
#' curl -O --output-dir /tmp/ "$URL"
#' sudo mkdir -p /opt/R/arm64/
#' sudo tar fxz /tmp/gfortran-12.0.1-20220312-is-darwin20-arm64.tar.xz -C /opt/R/arm64/ --strip-components 3
#' rm  /tmp/gfortran-12.0.1-20220312-is-darwin20-arm64.tar.xz
#' ```
#'
#' If users are on **R 4.1** with an **M-series** mac, then the _R_
#' sanitized _shell_ commands used are:
#'
#' ```sh
#' URL="https://mac.r-project.org/libs-arm64/gfortran-f51f1da0-darwin20.0-arm64.tar.gz"
#' curl -O --output-dir /tmp/ "$URL"
#' sudo mkdir -p /opt/R/arm64/
#' sudo tar fxz /tmp/gfortran-f51f1da0-darwin20.0-arm64.tar.gz -C /opt/R/arm64/ --strip-components 3
#' rm  /tmp/gfortran-f51f1da0-darwin20.0-arm64.tar.gz
#' ```
#'
#' Lastly, we modify the `PATH` environment variable to recognize the newly
#' installed software by adding into the `~/.Renviron` file the following:
#'
#' ```sh
#' touch ~/.Renviron
#' cat << "EOF" > ~./Renviron
#' ## macrtools - gfortran: start
#' PATH=${PATH}:/opt/R/arm64/gfortran/bin
#' ## macrtools - gfortran: end
#' EOF
#' ```
#'
#' @param verbose  Display messages indicating status
#' @param password Password for user account to install software. Default is
#'                 `NULL`.
#' @export
#' @rdname gfortran
gfortran_install <- function(password = getOption("macrtools.password"), verbose = TRUE) {
    assert_mac()
    assert_macos_supported()
    assert_r_version_supported()

    if(isTRUE(is_gfortran_installed())) {
        if(verbose) {
            cli_info(c(
                "gfortran is already installed at {.path {file.path(gfortran_install_location(), 'gfortran')}}.",
                "Version information: {.val {tryCatch(sys::as_text(sys::exec_internal('gfortran', '--version')$stdout), error = function(e) 'Unknown')}}"
            ))
        }
        return(invisible(TRUE))
    }

    if (verbose) {
        cli_info(c(
            "Preparing to download and install gfortran...",
            "Architecture: {.val {system_arch()}}",
            "R version: {.val {paste(R.version$major, R.version$minor, sep='.')}}",
            "Expected installation time: 2-5 minutes on a broadband connection."
        ))
    }

    # Figure out installation directory
    install_dir <- gfortran_install_location()

    # Establish a path
    path_gfortran_bin <- file.path(install_dir, "gfortran", "bin")
    path_variable <- paste0("${PATH}:", path_gfortran_bin)

    # Prompt for password if not found
    entered_password_gfortran <- force_password(password)

    # Ensure the installation location is valid.
    create_install_location(password = entered_password_gfortran)

    gfortran_status <- FALSE

    if(is_r_version("4.3") || is_r_version("4.4")) {
        gfortran_status <- install_gfortran_12_2_universal(
            password = entered_password_gfortran,
            verbose = verbose)
    } else if(is_r_version("4.0") || is_r_version("4.1") || is_r_version("4.2")) {
        if (is_x86_64()) {
            gfortran_status <- install_gfortran_82_mojave(
                password = entered_password_gfortran,
                verbose = verbose)
        } else if (is_aarch64()) {
            if (is_r_version("4.2")) {
                if (verbose) {
                    cli_info(c(
                        "Installing gfortran 12 for Apple Silicon with R 4.2",
                        "Source: {.url https://mac.r-project.org/tools/}",
                        "Target installation: {.path {install_location()}}"
                    ))
                }
                gfortran_status <- install_gfortran_12_arm(
                    password = entered_password_gfortran,
                    verbose = verbose)
            } else if(is_r_version("4.1")) {
                if (verbose) {
                    cli_info(c(
                        "Installing gfortran 11 for Apple Silicon with R 4.1",
                        "Source: {.url https://mac.r-project.org/libs-arm64/}",
                        "Target installation: {.path {install_location()}}"
                    ))
                }
                gfortran_status <- install_gfortran_11_arm(
                    password = entered_password_gfortran,
                    verbose = verbose)
            } else {
                cli_error(c(
                    "Unable to install gfortran for Apple Silicon (arm64/aarch64).",
                    "Official R support for Apple Silicon began in R 4.1.",
                    "Please upgrade R to version 4.1 or higher."
                ),
                advice = "Visit https://cran.r-project.org/bin/macosx/ to download a compatible R version for Apple Silicon.")
                return(invisible(FALSE))
            }
        } else {
            cli_error(c(
                "Unsupported macOS architecture: {.val {system_arch()}}",
                "Only Intel (x86_64) and Apple Silicon (arm64/aarch64) architectures are supported."
            ))
            return(invisible(FALSE))
        }
    }

    if(isFALSE(gfortran_status)) {
        cli_error(c(
            "Failed to install gfortran.",
            "Installation target: {.path {gfortran_install_location()}}",
            "Architecture: {.val {system_arch()}}",
            "R version: {.val {paste(R.version$major, R.version$minor, sep='.')}}"
        ),
        advice = "Please try to manually install following the instructions at: https://mac.thecoatlessprofessor.com/macrtools/reference/gfortran.html#installing-gfortran")
        return(invisible(FALSE))
    }

    renviron_gfortran_path(path_variable)

    if (verbose) {
        cli_success(c(
            "gfortran was successfully installed!",
            "Installation location: {.path {file.path(gfortran_install_location(), 'gfortran')}}",
            "PATH environment variable has been updated in your ~/.Renviron file.",
            "You may need to restart R for the changes to take effect."
        ))
    }

    return(invisible(TRUE))
}

#' @section Uninstalling `gfortran`:
#' The `gfortran_uninstall()` attempts to remove `gfortran` from
#' the default installation locations described in the details section.
#'
#' ### Uninstalling gfortran for Intel Macs
#'
#' We use the _R_ sanitized _shell_ command of:
#'
#' ```sh
#' sudo rm -rf /usr/local/gfortran /usr/local/bin/gfortran
#' ```
#'
#' These uninstall steps are based on:
#'
#' <https://gcc.gnu.org/wiki/GFortranBinariesMacOS>
#'
#'
#' ### Uninstalling gfortran for M1 or M2 Macs (`arm64`)
#'
#' For M1 or M2 Macs (`arm64`), we use the _R_ sanitized _shell_ command of:
#'
#' ```sh
#' sudo rm -rf /opt/R/arm64/gfortran/ /opt/R/arm64/bin/gfortran
#' ```
#'
#' This aligns with the default path used by CRAN for `arm64`:
#'
#' <https://mac.r-project.org/tools/>
#'
#' @export
#' @rdname gfortran
gfortran_uninstall <- function(password = getOption("macrtools.password"), verbose = TRUE) {
    assert_mac()

    if(isFALSE(is_gfortran_installed())) {
        if(verbose) {
            cli_info("gfortran is not installed.")
        }
        return(invisible(TRUE))
    }

    # Figure out installation directories
    install_dir <- gfortran_install_location()

    path_gfortran <- file.path(install_dir, "gfortran")
    path_bin_gfortran <- file.path(install_dir, "bin", "gfortran")

    if (verbose) {
        cli_info("Uninstalling gfortran...")
    }

    gfortran_uninstall_status <- shell_execute(
        paste0("rm -rf ", path_gfortran, " ", path_bin_gfortran),
        sudo = TRUE,
        password = password)

    gfortran_uninstall_clean <- identical(gfortran_uninstall_status, 0L)
    if(isFALSE(gfortran_uninstall_clean)) {
        cli_error(c(
            "We were not able to uninstall gfortran.",
            "Please try to manually uninstall using:",
            "{.url https://mac.thecoatlessprofessor.com/macrtools/reference/gfortran.html#uninstalling-gfortran}"
        ))
        return(invisible(FALSE))
    }

    if (verbose) {
        cli_success("gfortran was successfully uninstalled!")
    }

    return(invisible(gfortran_uninstall_clean))
}

#' @section Updating `gfortran`:
#' The `gfortran_update()` attempts to update the version of `gfortran` installed
#' using the provided `gfortran-update-sdk` inside of `/opt/R/arm64/gfortran/bin`.
#'
#' Please be advised that the update command only works for M1/M2 (`arm64`/`aarch64`)
#' users on _R_ 4.2 or above.
#'
#' The update command is issued using an _R_ sanitized version of the _shell_
#' command:
#'
#' ```sh
#' sudo /opt/R/arm64/gfortran/bin/gfortran-update-sdk
#' ```
#'
#' @export
#' @rdname gfortran
gfortran_update <- function(password = getOption("macrtools.password"), verbose = TRUE) {
    assert_mac()
    assert_aarch64()
    assert(is_gfortran_installed(), "gfortran must be installed")
    assert(is_r_version("4.2") || is_r_version("4.3") || is_r_version("4.4"),
           "R 4.2 or above is required for gfortran update")

    # Figure out installation directory
    install_dir <- install_location()

    path_gfortran_update <- file.path(install_dir, "gfortran", "bin", "gfortran-update-sdk")

    # Verify that the tool exists
    if (!file.exists(path_gfortran_update)) {
        cli_error("Could not find gfortran-update-sdk at {.path {path_gfortran_update}}")
        return(invisible(FALSE))
    }

    if (verbose) {
        cli_info("Updating gfortran...")
    }

    gfortran_update_status <- shell_execute(
        path_gfortran_update,
        sudo = TRUE,
        password = password)

    gfortran_update_clean <- identical(gfortran_update_status, 0L)
    if(isFALSE(gfortran_update_clean)) {
        cli_error(c(
            "We were not able to update gfortran.",
            "Please try to manually update using:",
            "{.url https://mac.thecoatlessprofessor.com/macrtools/reference/gfortran.html#updating-gfortran}"
        ))
        return(invisible(FALSE))
    }

    if (verbose) {
        cli_success("gfortran was successfully updated!")
    }

    return(invisible(gfortran_update_clean))
}


gfortran <- function(args) {
    out <- tryCatch(
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
install_gfortran_82_mojave <- function(password = getOption("macrtools.password"),
                                       verbose = TRUE) {

    # Key the necessary download steps
    gfortran_82_url <- "https://mac.r-project.org/tools/gfortran-8.2-Mojave.dmg"
    gfortran_dmg_name <- basename(gfortran_82_url)

    # Download the dmg file
    gfortran_path <- binary_download(gfortran_82_url, verbose = verbose)

    # Establish where in the dmg the installer package is
    path_to_pkg <-
        file.path(
            tools::file_path_sans_ext(gfortran_dmg_name),
            tools::file_path_sans_ext(gfortran_dmg_name),
            "gfortran.pkg"
        )

    # Install the dmg package
    success <- dmg_package_install(
        path_to_dmg = gfortran_path,
        pkg_location_in_dmg = path_to_pkg,
        password = password,
        verbose = verbose
    )

    if (!success) {
        cli_error("Error installing the gfortran package")
        return(FALSE)
    }

    # Remove unused gfortran file
    unlink(gfortran_path)

    return(invisible(TRUE))
}


install_gfortran_12_arm <- function(password = getOption("macrtools.password"),
                                    verbose = TRUE) {

    gfortran_12_url <- "https://mac.r-project.org/tools/gfortran-12.0.1-20220312-is-darwin20-arm64.tar.xz"

    # Figure out installation directories
    install_directory <- install_location()
    strip_levels <- install_strip_level()

    # Download tar
    path_to_tar <- binary_download(gfortran_12_url, verbose = verbose)

    # Install tar into the appropriate location
    tar_package_install(path_to_tar,
                        install_directory,
                        strip_levels,
                        password = password,
                        verbose = verbose)

}


install_gfortran_11_arm <- function(password = getOption("macrtools.password"),
                                    verbose = TRUE) {

    gfortran_11_url <- "https://mac.r-project.org/libs-arm64/gfortran-f51f1da0-darwin20.0-arm64.tar.gz"

    # Figure out installation directories
    install_directory <- install_location()
    strip_levels <- install_strip_level()

    # Download tar
    path_to_tar <- binary_download(gfortran_11_url, verbose = verbose)

    # Install tar into the appropriate location
    tar_package_install(path_to_tar,
                        install_directory,
                        strip_levels,
                        password = password,
                        verbose = verbose)

}


#' Install gfortran v12.2 universal binaries
#'
#' @details
#' Installs the `gfortran` v12.2 binaries for R version 4.3 for both
#' Intel and ARM-based macs.
#'
#' @noRd
install_gfortran_12_2_universal <- function(
        password = getOption("macrtools.password"),
        verbose = TRUE) {

    # URL
    gfortran_12_universal <- "https://mac.r-project.org/tools/gfortran-12.2-universal.pkg"

    # Download tar
    path_to_pkg <- binary_download(gfortran_12_universal, verbose = verbose)

    # Install pkg into the appropriate location
    pkg_install(path_to_pkg,
                "/",
                password = password,
                verbose = verbose)
}
