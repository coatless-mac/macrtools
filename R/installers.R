# Installation directories ----

install_strip_level <- function(arch = system_arch()) {
    base::switch(
        arch,
        "arm64" = 3,
        "aarch64" = 3,
        "x86_64" = 2,
        cli::cli_abort("{.pkg macrtools}: Architecture {.val {arch}} not recognized. Please ensure you are on either an Apple Silicon (arm64) or Intel (x86_64) system.")
    )
}

recipe_binary_install_strip_level <- function(arch = system_arch()) {
    if (is_r_version("4.3") || is_r_version("4.4")) {
        base::switch(
            arch,
            "arm64" = 3,
            "aarch64" = 3,
            "x86_64" = 3,
            cli::cli_abort("{.pkg macrtools}: Architecture {.val {arch}} not recognized. Please ensure you are on either an Apple Silicon (arm64) or Intel (x86_64) system.")
        )
    } else if (is_r_version("4.0") || is_r_version("4.1") || is_r_version("4.2")) {
        install_strip_level()
    } else {
        cli::cli_abort("{.pkg macrtools}: Unsupported R version. We only support recipe binary installation for R 4.0.x through 4.4.x.")
    }
}

install_location <- function(arch = system_arch()) {
    base::switch(
        arch,
        "arm64" = install_directory_arm64(),
        "aarch64" = install_directory_arm64(),
        "x86_64" = install_directory_x86_64(),
        cli::cli_abort("{.pkg macrtools}: Architecture {.val {arch}} not recognized. Please ensure you are on either an Apple Silicon (arm64) or Intel (x86_64) system.")
    )
}

recipe_binary_install_location <- function(arch = system_arch()) {
    if (is_r_version("4.3") || is_r_version("4.4")) {
        base::switch(
            arch,
            "arm64" = install_directory_arm64(),
            "aarch64" = install_directory_arm64(),
            "x86_64" = install_directory_x86_64_r_version_4_3(),
            cli::cli_abort("{.pkg macrtools}: Architecture {.val {arch}} not recognized. Please ensure you are on either an Apple Silicon (arm64) or Intel (x86_64) system.")
        )
    } else if (is_r_version("4.0") || is_r_version("4.1") || is_r_version("4.2")) {
        install_location()
    } else {
        cli::cli_abort("{.pkg macrtools}: Unsupported R version. We only support recipe binary installation for R 4.0.x through 4.4.x.")
    }
}

gfortran_install_location <- function(arch = system_arch()) {
    if (is_r_version("4.3") || is_r_version("4.4")) {
        "/opt"
    } else if (is_r_version("4.0") || is_r_version("4.1") || is_r_version("4.2")) {
        install_location()
    } else {
        cli::cli_abort("{.pkg macrtools}: Unsupported R version. We only support gfortran installation for R 4.0.x through 4.4.x.")
    }
}

create_install_location <- function(arch = system_arch(), password = base::getOption("macrtools.password")) {
    install_dir <- install_location(arch)

    # Verify installation directory exists. If it doesn't, create it.
    if (base::dir.exists(install_dir)) {
        return(base::invisible(TRUE))
    }

    # Get current directory permissions
    dir_perms <- base::tryCatch(
        base::paste(base::strsplit(base::system(base::paste('ls -ld', base::dirname(install_dir)), intern = TRUE), ' ')[[1]][1:3], collapse = ' '),
        error = function(e) "Unknown"
    )

    current_user <- base::Sys.info()['user']

    cli::cli_alert_info("{.pkg macrtools}: Creating installation directory for binaries.")
    cli::cli_bullets(c(
        "Architecture: {.val {arch}}",
        "Target directory: {.path {install_dir}}",
        "Permission level: Administrative privileges required",
        "Current user: {.val {current_user}}"
    ))
    cli::cli_text("") # Add spacing

    dir_creation_status <- shell_execute(
        base::paste("mkdir", "-p", install_dir),
        sudo = TRUE,
        password = password)

    dir_creation_clean <- base::identical(dir_creation_status, 0L)

    if (!dir_creation_clean) {
        cli::cli_abort(c(
            "{.pkg macrtools}: Failed to create installation directory.",
            "Directory: {.path {install_dir}}",
            "Status code: {.val {dir_creation_status}}",
            "Architecture: {.val {arch}}",
            "Current permissions: {.val {dir_perms}}",
            "i" = "Check that you have sufficient administrative privileges and the parent directory exists."
        ))
    } else {
        # Get owner of new directory
        new_owner <- base::tryCatch(
            base::paste(base::strsplit(base::system(base::paste('ls -ld', install_dir), intern = TRUE), ' ')[[1]][3], collapse = ' '),
            error = function(e) "Unknown"
        )

        cli::cli_alert_success("{.pkg macrtools}: Installation directory created successfully.")
        cli::cli_bullets(c(
            "Directory: {.path {install_dir}}",
            "Owner: {.val {new_owner}}"
        ))
        cli::cli_text("") # Add spacing
    }

    return(base::invisible(dir_creation_clean))
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
        temp_dir <- base::tempdir()

        cli::cli_alert_info("{.pkg macrtools}: Starting download of binary package.")
        cli::cli_bullets(c(
            "Source URL: {.url {url}}",
            "File name: {.file {binary_file_name}}",
            "Temporary directory: {.path {temp_dir}}"
        ))
        cli::cli_text("") # Add spacing
    }

    # Configure temporary file location
    save_location <- base::file.path(base::tempdir(), binary_file_name)

    # Create progress bar
    if (verbose) {
        pb_id <- cli::cli_progress_bar(
            format = "{.pkg macrtools}: {.strong Downloading package} {.file {binary_file_name}} {.progress_bar} {.percent} {.spinner}",
            format_done = "{.pkg macrtools}: {.strong Download complete} {cli::symbol$tick}",
            total = 100
        )
    }

    # Download with progress and error handling
    download_start_time <- base::Sys.time()
    download_result <- base::tryCatch({
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
        if (verbose) cli::cli_progress_done(pb_id)

        # Extract status code if available
        status_code <- base::attr(e, 'status_code', exact = TRUE)
        status_info <- if(base::is.null(status_code)) 'Unknown' else status_code

        cli::cli_abort(c(
            "x" = "{.pkg macrtools}: Failed to download binary package.",
            ">" = "URL: {.url {url}}",
            ">" = "Error message: {.val {e$message}}",
            ">" = "Status code: {.val {status_info}}",
            "i" = "Check your internet connection or try again later. The server may be temporarily unavailable."
        ))
        FALSE
    })

    download_duration <- base::difftime(base::Sys.time(), download_start_time, units = "secs")

    if (!download_result) {
        return(NULL)
    }

    if (verbose) {
        file_size <- base::file.info(save_location)$size
        file_size_mb <- base::round(file_size / (1024 * 1024), 2)
        duration_sec <- base::round(base::as.numeric(download_duration), 1)
        avg_speed <- base::round(file_size_mb/(base::as.numeric(download_duration)/60), 2)

        cli::cli_progress_done(pb_id)
        cli::cli_alert_success("{.pkg macrtools}: Binary package downloaded successfully.")
        cli::cli_bullets(c(
            "Saved to: {.path {save_location}}",
            "File size: {.val {file_size_mb}} MB",
            "Download time: {.val {duration_sec}} seconds",
            "Average speed: {.val {avg_speed}} MB/min"
        ))
        cli::cli_text("") # Add spacing
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

    norm_install_dir <- base::shQuote(base::normalizePath(install_directory))
    binary_file_name <- base::basename(path_to_tar)

    if (verbose) {
        cli::cli_alert_info("{.pkg macrtools}: Installing package from tar archive.")
        cli::cli_bullets(c(
            "File: {.file {binary_file_name}}",
            "Destination: {.path {install_directory}}",
            "Strip levels: {.val {strip_levels}}"
        ))
        cli::cli_text("") # Add spacing
    }

    # Step Two: Install the package using tar with stdin redirect
    cmd <- base::paste0("tar fxj ", path_to_tar," -C ", norm_install_dir, " --strip-components ", strip_levels)
    status <- shell_execute(cmd, sudo = sudo, password = password, verbose = verbose)

    # Verify installation is okay:
    if (status < 0) {
        cli::cli_abort("{.pkg macrtools}: Failed to install from {.path {path_to_tar}}")
    }

    if (verbose) {
        cli::cli_alert_info("{.pkg macrtools}: Removing temporary tar file: {.file {binary_file_name}}")
        cli::cli_text("") # Add spacing
    }

    # Step Three: Remove the tar package
    base::unlink(path_to_tar)

    if (verbose && status == 0) {
        cli::cli_alert_success("{.pkg macrtools}: Installation completed successfully!")
        cli::cli_text("") # Add spacing
    }

    status == 0
}

# DMG Installation ----

dmg_package_install <- function(path_to_dmg,
                                pkg_location_in_dmg,
                                password = NULL,
                                verbose = TRUE) {

    volume_with_extension <- base::basename(path_to_dmg)
    bare_volume <- tools::file_path_sans_ext(volume_with_extension)

    if (verbose) {
        cli::cli_alert_info("{.pkg macrtools}: Mounting disk image.")
        cli::cli_bullets(c(
            "Disk image: {.file {volume_with_extension}}",
            "Volume name: {.val {bare_volume}}"
        ))
        cli::cli_text("") # Add spacing
    }

    cmd <- base::paste("hdiutil attach", base::shQuote(path_to_dmg), "-nobrowse -quiet")
    mount_status <- shell_execute(cmd, sudo = FALSE)

    if (mount_status != 0) {
        cli::cli_abort(c(
            "{.pkg macrtools}: Failed to mount disk image.",
            "Disk image: {.file {volume_with_extension}}",
            "Status code: {.val {mount_status}}"
        ))
        return(FALSE)
    }

    if (verbose) {
        cli::cli_alert_info("{.pkg macrtools}: Installing package from disk image.")
        cli::cli_bullets(c(
            "Package: {.file {bare_volume}}",
            "Package location: {.path {pkg_location_in_dmg}}"
        ))
        cli::cli_text("") # Add spacing
    }

    cmd <- base::paste(
        "sudo",
        "-kS",
        "installer",
        "-pkg",
        base::paste0("'/Volumes/", pkg_location_in_dmg, "'"),
        "-target",
        "/"
    )
    install_status <- shell_execute(cmd, sudo = TRUE, password = password)

    if (install_status != 0) {
        cli::cli_abort(c(
            "{.pkg macrtools}: Failed to install package from disk image.",
            "Disk image: {.file {volume_with_extension}}",
            "Status code: {.val {install_status}}"
        ))
        # Attempt to unmount anyway
        cmd <- base::paste("hdiutil", "detach", base::shQuote(base::file.path("/Volumes", bare_volume)))
        shell_execute(cmd, sudo = FALSE)
        return(FALSE)
    }

    if (verbose) {
        cli::cli_alert_info("{.pkg macrtools}: Unmounting disk image.")
        cli::cli_bullets(c(
            "Volume: {.file {bare_volume}}"
        ))
        cli::cli_text("") # Add spacing
    }

    cmd <- base::paste("hdiutil", "detach", base::shQuote(base::file.path("/Volumes", bare_volume)))
    status <- shell_execute(cmd, sudo = FALSE)

    if (status != 0) {
        cli::cli_alert_warning(c(
            "{.pkg macrtools}: Failed to unmount disk image.",
            "i" = "You may need to unmount it manually."
        ))
        cli::cli_text("") # Add spacing
    }

    if (verbose && install_status == 0) {
        cli::cli_alert_success("{.pkg macrtools}: Installation completed successfully!")
        cli::cli_text("") # Add spacing
    }

    install_status == 0
}

pkg_install <- function(path_to_pkg,
                        target_location = "/",
                        password = NULL,
                        verbose = TRUE) {

    package_name_with_extension <- base::basename(path_to_pkg)
    package_name <- tools::file_path_sans_ext(package_name_with_extension)

    if (verbose) {
        cli::cli_alert_info("{.pkg macrtools}: Installing package.")
        cli::cli_bullets(c(
            "Package: {.file {package_name}}",
            "Path: {.path {path_to_pkg}}",
            "Target: {.path {target_location}}"
        ))
        cli::cli_text("") # Add spacing
    }

    cmd <- base::paste(
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
        cli::cli_abort(c(
            "{.pkg macrtools}: Failed to install package.",
            "Package: {.file {package_name}}",
            "Status code: {.val {status}}"
        ))
        return(FALSE)
    }

    if (verbose) {
        cli::cli_alert_success("{.pkg macrtools}: Package installation completed successfully!")
        cli::cli_text("") # Add spacing
    }

    status == 0
}
