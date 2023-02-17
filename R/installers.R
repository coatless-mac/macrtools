install_location = function(arch) {
    if (missing(arch)) {
        arch = system_arch()
    }

    # Determine the correct installation path based on arch type
    if( grepl("(arm64)|(aarch64)", arch, fixed = TRUE)) {
        strip_levels = 3
        install_directory = "/opt/R/arm64"
    } else if (grepl("x86_64", arch, fixed = TRUE)) {
        strip_levels = 2
        install_directory = "/usr/local"
    } else {
        stop("Arch type not recognized. Please make sure you are on either an `arm64` or `x86_64` system.")
    }

    list(strip_levels = strip_levels,
         install_directory = install_directory)
}

download_tar_package = function(binary_url) {

    binary_file_name = basename(binary_url)

    # Three step procedure:
    cat("Downloading tar: ", binary_url, "...\n")

    # Step One:Download the package into the temporary directory
    save_location = file.path(tempdir(), binary_file_name)
    utils::download.file(
        binary_url,
        save_location
    )

    save_location
}


install_tar_package = function(path_to_tar,
                               install_directory,
                               strip_levels,
                               sudo = TRUE,
                               password = NULL,
                               verbose = TRUE) {

    install_directory = shQuote(normalizePath(install_directory))

    binary_file_name = basename(path_to_tar)

    cat("Installing: ", binary_file_name, " into ", install_directory ," ...\n")

    # Step Two: Install the package using tar with stdin redirect
    cmd = paste0("tar fxj ", path_to_tar," -C ", install_directory, " --strip ", strip_levels)
    status = shell_execute(cmd, sudo = sudo, password = password)

    # Verify installation is okay:
    if (status < 0)
        stop("Failed to install from ", path_to_tar)

    cat("\nRemoving temporary tar file: ", binary_file_name, "...\n")

    # Step Three: Remove the tar package
    unlink(path_to_tar)

    status == 0
}


install_dmg_package = function(path_to_dmg,
                               pkg_location_in_dmg,
                               password = NULL,
                               verbose = TRUE) {

    volume_name = basename(path_to_dmg)

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
        paste0("'/Volumes/", pkg_location_in_dmg, "'"),
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
