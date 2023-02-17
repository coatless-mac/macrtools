#' @include utils.R shell.R
NULL

# Installation directories ----
install_directory_x86_64 = function() {
    "/usr/local"
}
install_directory_arm64 = function() {
    "/opt/R/arm64"
}

install_strip_level = function(arch = system_arch()) {
    switch(
        arch,
        "arm64" = 3,
        "aarch64" = 3,
        "x86_64" = 2,
        stop("`arch` type not recognized. Please make sure you are on either an `arm64` or `x86_64` system.")
    )
}

install_location = function(arch = system_arch()) {
    switch(
        arch,
        "arm64" = install_directory_arm64(),
        "aarch64" = install_directory_arm64(),
        "x86_64" = install_directory_x86_64(),
        stop("`arch` type not recognized. Please make sure you are on either an `arm64` or `x86_64` system.")
     )
}


# Tar Installation ----

#' Download and Install Tar Packages
#'
#' Downloads or installs a tar package onto a user's computer.
#'
#' @param url              A link containing the binary tar file to download.
#' @param binary_file_name Name of the binary file to save
#'
#' @return
#' The downloaded tar file path
#'
#' @description
#' The _R_ temporary directory is created using [tempdir()] that
#' consults the environment values of [TMPDIR], [TMP], or [TEMP] to establish
#' the path. We need to use a temporary directory as `root` is not guaranteed
#' to have access to user's files.
#'
#' We need to use `sudo` access to unpack the binary into the low-level system
#' holding area.
#'
#' @export
#' @rdname tar-package
tar_package_download = function(url,  binary_file_name = basename(url), verbose = TRUE) {

    # Three step procedure:
    cat("Downloading tar: ", url, "...\n")

    # Step One:Download the package into the temporary directory
    save_location = file.path(tempdir(), binary_file_name)
    utils::download.file(
        url,
        save_location,
        quiet = !verbose
    )

    save_location
}


#'
#' @param path_to_tar Location of where the tar file is
#' @param install_directory Location of where to unpack or extract the tar file.
#' @param strip_levels Remove nesting inside of the `tar` file
#' @param sudo     Run the command as `root` through `sudo`. Default `TRUE`
#' @param password User password to use to enter `sudo` mode.
#' @param verbose  Display status messages
#' @export
#' @rdname tar-package
tar_package_install = function(path_to_tar,
                               install_directory,
                               strip_levels,
                               sudo = TRUE,
                               password = NULL,
                               verbose = TRUE) {

    install_directory = shQuote(normalizePath(install_directory))

    binary_file_name = basename(path_to_tar)

    if (verbose)
        cat("Installing: ", binary_file_name, " into ", install_directory ," ...\n")

    # Step Two: Install the package using tar with stdin redirect
    cmd = paste0("tar fxj ", path_to_tar," -C ", install_directory, " --strip ", strip_levels)
    status = shell_execute(cmd, sudo = sudo, password = password, verbose = verbose)

    # Verify installation is okay:
    if (status < 0)
        stop("Failed to install from ", path_to_tar)

    if (verbose)
        cat("\nRemoving temporary tar file: ", binary_file_name, "...\n")

    # Step Three: Remove the tar package
    unlink(path_to_tar)

    status == 0
}

# DMG Installation ----

dmg_package_install = function(path_to_dmg,
                               pkg_location_in_dmg,
                               password = NULL,
                               verbose = TRUE) {

    volume_with_extension = basename(path_to_dmg)
    bare_volume = tools::file_path_sans_ext(volume_with_extension)

    if (verbose) message("Mounting ", volume_with_extension)

    cmd = paste("hdiutil attach", shQuote(path_to_dmg), "-nobrowse -quiet")
    shell_execute(cmd, sudo = FALSE)

    if (verbose) {
        message("Installing ", bare_volume, "...")
        message("You may be prompted for your password ...")
    }
    cmd = paste(
        "sudo",
        "-kS",
        "installer",
        "-pkg",
        paste0("'/Volumes/", pkg_location_in_dmg, "'"),
        "-target",
        "/"
    )
    shell_execute(cmd, sudo = TRUE, password = password)

    if (verbose) {
        message("Unmounting ", bare_volume, "...")
    }
    cmd = paste("hdiutil", "detach", shQuote(file.path("/Volumes", bare_volume)))
    status = shell_execute(cmd, sudo = FALSE)

    status == 0
}
