#' @include utils.R shell.R
NULL

# Installation directories ----

install_strip_level = function(arch = system_arch()) {
    switch(
        arch,
        "arm64" = 3,
        "aarch64" = 3,
        "x86_64" = 2,
        stop("`arch` type not recognized. Please make sure you are on either an `arm64` or `x86_64` system.")
    )
}

recipe_binary_install_strip_level = function(arch = system_arch()) {
    if (is_r_version("4.3") || is_r_version("4.4")) {
        switch(
            arch,
            "arm64" = 3,
            "aarch64" = 3,
            "x86_64" = 3,
            stop("`arch` type not recognized. Please make sure you are on either an `arm64` or `x86_64` system.")
        )
    } else if (is_r_version("4.0") | is_r_version("4.1") | is_r_version("4.2")) {
        install_strip_level()
    } else {
        stop("We do not yet support recipe binary installation for the current version of R.")
    }
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

recipe_binary_install_location = function(arch = system_arch()) {
    if (is_r_version("4.3") || is_r_version("4.4")) {
        switch(
            arch,
            "arm64" = install_directory_arm64(),
            "aarch64" = install_directory_arm64(),
            "x86_64" = install_directory_x86_64_r_version_4_3(),
            stop("`arch` type not recognized. Please make sure you are on either an `arm64` or `x86_64` system.")
        )
    } else if (is_r_version("4.0") | is_r_version("4.1") | is_r_version("4.2")) {
        install_location()
    } else {
        stop("We do not yet support recipe binary installation for the current version of R.")
    }
}

gfortran_install_location = function(arch = system_arch()) {
    if (is_r_version("4.3") || is_r_version("4.4")) {
        "/opt"
    } else if (is_r_version("4.0") | is_r_version("4.1") | is_r_version("4.2")) {
        install_location()
    } else {
        stop("We do not yet support gfortran installation for the current version of R.")
    }
}

create_install_location = function(arch = system_arch(), password = getOption("macrtools.password")) {

    install_dir = install_location(arch)

    # Verify installation directory exists. If it doesn't, create it.
    if (dir.exists(install_dir)) return(invisible(TRUE))

    dir_creation_status = shell_execute(
            paste("mkdir", "-p", install_dir ),
            sudo = TRUE,
            password = password)

    dir_creation_clean = identical(dir_creation_status, 0L)

    return(invisible(dir_creation_clean))
}


# Tar Installation ----

#' Download Binary Packages
#'
#' Downloads a binary package onto a user's computer into the temporary directory.
#'
#' @param url              A link containing the binary file to download.
#' @param binary_file_name Name of the binary file to save. Default `basname(url)`
#' @param verbose          Display a status messages. Default `true`
#'
#' @return
#' The file path for the binary file in the temporary _R_ directory
#'
#' @description
#' The _R_ temporary directory is created using [tempdir()] that
#' consults the environment values of [TMPDIR], [TMP](TMPDIR), or [TEMP](TMPDIR) to establish
#' the path. We need to use a temporary directory as `root` is not guaranteed
#' to have access to user's files.
#'
#' @export
binary_download = function(url,  binary_file_name = basename(url), verbose = TRUE) {

    # Three step procedure:
    cat("Downloading binary: ", url, "...\n")

    # Step One:Download the package into the temporary directory
    save_location = file.path(tempdir(), binary_file_name)
    utils::download.file(
        url,
        save_location,
        quiet = !verbose
    )

    save_location
}


#' Install Binary Package in a Tar Format
#'
#' Unpacks the Tar package and places it into a system library
#'
#' @param path_to_tar Location of where the tar file is
#' @param install_directory Location of where to unpack or extract the tar file.
#' @param strip_levels Remove nesting inside of the `tar` file
#' @param sudo     Run the command as `root` through `sudo`. Default `TRUE`
#' @param password User password to use to enter `sudo` mode.
#' @param verbose  Display status messages
#'
#' @description
#' The use of `sudo` is required to unpack the binary file
#' into the low-level system area.
#'
#' @export
tar_package_install = function(path_to_tar,
                               install_directory,
                               strip_levels,
                               sudo = TRUE,
                               password = NULL,
                               verbose = TRUE) {

    install_directory = shQuote(normalizePath(install_directory))

    binary_file_name = basename(path_to_tar)

    if (verbose)
        cat("Installing:", binary_file_name, "into", install_directory ,"...\n")

    # Step Two: Install the package using tar with stdin redirect
    cmd = paste0("tar fxj ", path_to_tar," -C ", install_directory, " --strip-components ", strip_levels)
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

    if (verbose) message("Mounting ", volume_with_extension, " ...")

    cmd = paste("hdiutil attach", shQuote(path_to_dmg), "-nobrowse -quiet")
    shell_execute(cmd, sudo = FALSE)

    if (verbose) {
        message("Installing ", bare_volume, " ...")
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

pkg_install = function(path_to_pkg,
                       target_location = "/",
                       password = NULL,
                       verbose = TRUE) {

    package_name_with_extension = basename(path_to_pkg)
    package_name = tools::file_path_sans_ext(package_name_with_extension)

    if (verbose) {
        message("Installing ", package_name, " ...")
    }
    cmd = paste(
        "sudo",
        "-kS",
        "installer",
        "-pkg",
        path_to_pkg,
        "-target",
        target_location
    )
    status = shell_execute(cmd, sudo = TRUE, password = password)

    status == 0
}

