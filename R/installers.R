#' @include utils.R shell.R cli-custom.R
NULL

# Installation directories ----

install_strip_level <- function(arch = system_arch()) {
    switch(
        arch,
        "arm64" = 3,
        "aarch64" = 3,
        "x86_64" = 2,
        cli_error("Architecture {.val {arch}} not recognized. Please ensure you are on either an Apple Silicon (arm64) or Intel (x86_64) system.")
    )
}

recipe_binary_install_strip_level <- function(arch = system_arch()) {
    if (is_r_version("4.3") || is_r_version("4.4")) {
        switch(
            arch,
            "arm64" = 3,
            "aarch64" = 3,
            "x86_64" = 3,
            cli_error("Architecture {.val {arch}} not recognized. Please ensure you are on either an Apple Silicon (arm64) or Intel (x86_64) system.")
        )
    } else if (is_r_version("4.0") || is_r_version("4.1") || is_r_version("4.2")) {
        install_strip_level()
    } else {
        cli_error("Unsupported R version. We only support recipe binary installation for R 4.0.x through 4.4.x.")
    }
}

install_location <- function(arch = system_arch()) {
    switch(
        arch,
        "arm64" = install_directory_arm64(),
        "aarch64" = install_directory_arm64(),
        "x86_64" = install_directory_x86_64(),
        cli_error("Architecture {.val {arch}} not recognized. Please ensure you are on either an Apple Silicon (arm64) or Intel (x86_64) system.")
    )
}

recipe_binary_install_location <- function(arch = system_arch()) {
    if (is_r_version("4.3") || is_r_version("4.4")) {
        switch(
            arch,
            "arm64" = install_directory_arm64(),
            "aarch64" = install_directory_arm64(),
            "x86_64" = install_directory_x86_64_r_version_4_3(),
            cli_error("Architecture {.val {arch}} not recognized. Please ensure you are on either an Apple Silicon (arm64) or Intel (x86_64) system.")
        )
    } else if (is_r_version("4.0") || is_r_version("4.1") || is_r_version("4.2")) {
        install_location()
    } else {
        cli_error("Unsupported R version. We only support recipe binary installation for R 4.0.x through 4.4.x.")
    }
}

gfortran_install_location <- function(arch = system_arch()) {
    if (is_r_version("4.3") || is_r_version("4.4")) {
        "/opt"
    } else if (is_r_version("4.0") || is_r_version("4.1") || is_r_version("4.2")) {
        install_location()
    } else {
        cli_error("Unsupported R version. We only support gfortran installation for R 4.0.x through 4.4.x.")
    }
}

create_install_location <- function(arch = system_arch(), password = getOption("macrtools.password")) {
    install_dir <- install_location(arch)

    # Verify installation directory exists. If it doesn't, create it.
    if (base::dir.exists(install_dir)) {
        return(invisible(TRUE))
    }

    cli_info(c(
        "Creating installation directory for binaries.",
        "Architecture: {.val {arch}}",
        "Target directory: {.path {install_dir}}",
        "Permission level: Administrative privileges required",
        "Current user: {.val {Sys.info()['user']}}"
    ))

    dir_creation_status <- shell_execute(
        base::paste("mkdir", "-p", install_dir),
        sudo = TRUE,
        password = password)

    dir_creation_clean <- base::identical(dir_creation_status, 0L)

    if (!dir_creation_clean) {
        cli_error(c(
            "Failed to create installation directory.",
            "Directory: {.path {install_dir}}",
            "Status code: {.val {dir_creation_status}}",
            "Architecture: {.val {arch}}",
            "Current permissions: {.val {base::paste(base::strsplit(base::system(paste('ls -ld', base::dirname(install_dir)), intern = TRUE), ' ')[[1]][1:3], collapse = ' ')}}"
        ),
        advice = "Check that you have sufficient administrative privileges and the parent directory exists.")
    } else {
        cli_success(c(
            "Installation directory created successfully.",
            "Directory: {.path {install_dir}}",
            "Owner: {.val {base::paste(base::strsplit(base::system(paste('ls -ld', install_dir), intern = TRUE), ' ')[[1]][3], collapse = ' ')}}"
        ))
    }

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
#' @param mode             Mode to use for download. Default is "wb" (binary mode)
#' @param timeout          Timeout in seconds. Default is 600 (10 minutes)
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
binary_download <- function(url, binary_file_name = base::basename(url), verbose = TRUE,
                            mode = "wb", timeout = 600) {
    if (verbose) {
        cli_info(c(
            "Starting download of binary package.",
            "Source URL: {.url {url}}",
            "File name: {.file {binary_file_name}}",
            "Temporary directory: {.path {base::tempdir()}}",
            "Internet connection: {.val {Sys.getenv('R_NETWORK_STATUS', unset = 'Unknown')}}"
        ))
    }

    # Configure temporary file location
    save_location <- base::file.path(base::tempdir(), binary_file_name)

    # Create progress bar
    if (verbose) {
        pb_id <- cli_process_start("Downloading package", msg = binary_file_name)
    }

    # Download with progress and error handling
    download_start_time <- Sys.time()
    download_result <- tryCatch({
        utils::download.file(
            url,
            save_location,
            quiet = !verbose,
            mode = mode,
            cacheOK = FALSE,
            method = "auto",
            timeout = timeout
        )
        TRUE
    }, error = function(e) {
        if (verbose) cli_process_done(pb_id, msg = "Download failed")
        cli_error(c(
            "Failed to download binary package.",
            "URL: {.url {url}}",
            "Error message: {.val {e$message}}",
            "Status code (if available): {.val {attr(e, 'status_code', exact = TRUE) %||% 'Unknown'}}"
        ),
        advice = "Check your internet connection or try again later. The server may be temporarily unavailable.")
        FALSE
    })

    download_duration <- difftime(Sys.time(), download_start_time, units = "secs")

    if (!download_result) {
        return(NULL)
    }

    if (verbose) {
        file_size <- file.info(save_location)$size
        file_size_mb <- round(file_size / (1024 * 1024), 2)
        cli_process_done(pb_id, msg = "Download complete")
        cli_success(c(
            "Binary package downloaded successfully.",
            "Saved to: {.path {save_location}}",
            "File size: {.val {file_size_mb}} MB",
            "Download time: {.val {round(as.numeric(download_duration), 1)}} seconds",
            "Average speed: {.val {round(file_size_mb/(as.numeric(download_duration)/60), 2)}} MB/min"
        ))
    }

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
tar_package_install <- function(path_to_tar,
                                install_directory,
                                strip_levels,
                                sudo = TRUE,
                                password = NULL,
                                verbose = TRUE) {

    install_directory <- shQuote(normalizePath(install_directory))
    binary_file_name <- basename(path_to_tar)

    if (verbose) {
        cli_info("Installing: {.file {binary_file_name}} into {.path {install_directory}}")
    }

    # Step Two: Install the package using tar with stdin redirect
    cmd <- paste0("tar fxj ", path_to_tar," -C ", install_directory, " --strip-components ", strip_levels)
    status <- shell_execute(cmd, sudo = sudo, password = password, verbose = verbose)

    # Verify installation is okay:
    if (status < 0) {
        cli_error("Failed to install from {.path {path_to_tar}}")
    }

    if (verbose) {
        cli_info("Removing temporary tar file: {.file {binary_file_name}}")
    }

    # Step Three: Remove the tar package
    unlink(path_to_tar)

    if (verbose && status == 0) {
        cli_success("Installation completed successfully.")
    }

    status == 0
}

# DMG Installation ----

dmg_package_install <- function(path_to_dmg,
                                pkg_location_in_dmg,
                                password = NULL,
                                verbose = TRUE) {

    volume_with_extension <- basename(path_to_dmg)
    bare_volume <- tools::file_path_sans_ext(volume_with_extension)

    if (verbose) {
        cli_info("Mounting disk image: {.file {volume_with_extension}}")
    }

    cmd <- paste("hdiutil attach", shQuote(path_to_dmg), "-nobrowse -quiet")
    mount_status <- shell_execute(cmd, sudo = FALSE)

    if (mount_status != 0) {
        cli_error("Failed to mount disk image: {.file {volume_with_extension}}")
        return(FALSE)
    }

    if (verbose) {
        cli_info("Installing package: {.file {bare_volume}}")
    }

    cmd <- paste(
        "sudo",
        "-kS",
        "installer",
        "-pkg",
        paste0("'/Volumes/", pkg_location_in_dmg, "'"),
        "-target",
        "/"
    )
    install_status <- shell_execute(cmd, sudo = TRUE, password = password)

    if (install_status != 0) {
        cli_error("Failed to install package from disk image.")
        # Attempt to unmount anyway
        cmd <- paste("hdiutil", "detach", shQuote(file.path("/Volumes", bare_volume)))
        shell_execute(cmd, sudo = FALSE)
        return(FALSE)
    }

    if (verbose) {
        cli_info("Unmounting disk image: {.file {bare_volume}}")
    }

    cmd <- paste("hdiutil", "detach", shQuote(file.path("/Volumes", bare_volume)))
    status <- shell_execute(cmd, sudo = FALSE)

    if (status != 0) {
        cli_warning("Failed to unmount disk image. You may need to unmount it manually.")
    }

    if (verbose && install_status == 0) {
        cli_success("Installation completed successfully.")
    }

    install_status == 0
}

pkg_install <- function(path_to_pkg,
                        target_location = "/",
                        password = NULL,
                        verbose = TRUE) {

    package_name_with_extension <- basename(path_to_pkg)
    package_name <- tools::file_path_sans_ext(package_name_with_extension)

    if (verbose) {
        cli_info("Installing package: {.file {package_name}}")
    }

    cmd <- paste(
        "sudo",
        "-kS",
        "installer",
        "-pkg",
        path_to_pkg,
        "-target",
        target_location
    )
    status <- shell_execute(cmd, sudo = TRUE, password = password)

    if (status != 0) {
        cli_error("Failed to install package: {.file {package_name}}")
        return(FALSE)
    }

    if (verbose) {
        cli_success("Package installation completed successfully.")
    }

    status == 0
}
