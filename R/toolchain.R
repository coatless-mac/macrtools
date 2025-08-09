#' Install and Uninstall the macOS R Toolchain
#'
#' The `macos_rtools_install()` function aims to install all required dependencies
#' for the macOS R compilation toolchain. Meanwhile, the `macros_rtools_uninstall()`
#' function aims to remove any installed files from your computer.
#'
#' @param password Password for user account to request `sudo` access.
#' @param verbose  Describe each step taken. Default `TRUE`
#' @details
#'
#' The macOS R compilation toolchain consists of:
#'
#' 1. Xcode CLI
#' 2. gfortran
#' 3. A series of binary packages from the [`recipes`](https://github.com/R-macos/recipes) system to compile R.
#'
#' The `mac_rtools_install()` function attempts to install each of the required
#' components. If we detect that the Xcode.app IDE is installed, we'll skip
#' attempting to install the Xcode CLI software.
#'
#' Meanwhile, the `mac_rtools_uninstall()` function aims to
#' delete or uninstall the Xcode CLI and gfortran binaries. At the present moment,
#' there is no support for uninstalling the binary packages from `recipes`.
#'
#' @section Headless or Unattended Installation:
#'
#' For an installation that does not require a human in the loop, please set
#' the environment variable `macrtools.password` for the user profile:
#'
#' ```r
#' Sys.setenv("macrtools.password" = "your-password-here")
#' ```
#'
#' @rdname macos-rtools
#' @export
#' @examples
#'
#' \dontrun{
#'
#' # Install all tools required for compiling code on macOS with R
#' macos_rtools_install()
#'
#' # Enter a password to avoid being prompted!
#' macos_rtools_install(password = "hello-compiled-code-world")
#'
#' }
macos_rtools_install <- function(
        password = base::getOption("macrtools.password"),
        verbose = TRUE
) {
    # Initial system check
    assert_mac()
    assert_macos_supported()
    assert_r_version_supported()

    # Installation heading and components list - proper formatting
    cli::cli_h3("Installing macOS R Development Toolchain")
    cli::cli_text("This process will install all required components:")
    cli::cli_ul(c(
        "Xcode Command Line Tools - Apple's development utilities",
        "gfortran - GNU Fortran compiler required for many scientific packages",
        "r-base-dev - Essential libraries from the R-macOS Recipes project"
    ))
    cli::cli_text("") # Add spacing

    # System information with temporary variables
    os_version <- shell_mac_version()
    os_release <- base::Sys.info()['release']
    arch <- system_arch()
    r_version <- base::paste(base::R.version$major, base::R.version$minor, sep='.')

    cli::cli_alert_info("System requirements check:")
    cli::cli_bullets(c(
        "Operating system: {.val {os_version}} ({.val {os_release}})",
        "Architecture: {.val {arch}}",
        "R version: {.val {r_version}}",
        "Administrator privileges: Required"
    ))
    cli::cli_text("") # Add spacing

    cli::cli_alert_info("Installation prerequisites:")
    cli::cli_bullets(c(
        "Ensure you have a stable internet connection",
        "Connect your computer to a power source",
        "Estimated installation time: 17-25 minutes",
        "Required disk space: Approximately 6-9 GB"
    ))
    cli::cli_text("") # Add spacing

    entered_password <- password
    if(base::is.null(entered_password)) {
        cli::cli_alert_info("Administrative privileges are required for installation.")
        entered_password <- askpass::askpass("Please enter your administrator password:")
    }

    describe_steps <- base::isTRUE(verbose)

    # Create a detailed progress bar
    if (verbose) {
        pb_id <- cli::cli_progress_bar(
            format = "{.pkg macrtools}: {.strong Installing R development toolchain} {cli::pb_bar} {cli::pb_percent} {cli::pb_spin}",
            format_done = "{.pkg macrtools}: {.strong Installation complete} {cli::symbol$tick}",
            total = 100
        )

        current_time <- base::format(base::Sys.time(), '%Y-%m-%d %H:%M:%S')
        cli::cli_alert_info("Installation process started at: {.val {current_time}}")
        cli::cli_text("") # Add spacing
    }

    # COMPONENT 1: Xcode Command Line Tools
    cli::cli_h3("Component 1 of 3: Xcode Command Line Tools")
    cli::cli_text("Apple's development utilities providing compilers and build tools")
    cli::cli_ul(c(
        "Includes: gcc, make, git, clang, and other essential development tools",
        "Location: /Library/Developer/CommandLineTools",
        "Size: ~1.5 GB"
    ))
    cli::cli_text("") # Add spacing

    # Step 1: Xcode CLI
    result_xcode <- TRUE
    if(!is_xcode_app_installed()) {
        if(!is_xcode_cli_installed()) {
            if (verbose) {
                cli::cli_progress_update(pb_id, 0.1)
                cli::cli_alert_info("{.pkg macrtools}: Need to install Xcode Command Line Tools.")
                cli::cli_bullets(c(
                    "Source: Apple Software Update",
                    "Installation method: softwareupdate command",
                    "Status: Not installed",
                    "Estimated time: 10-15 minutes"
                ))
                cli::cli_text("") # Add spacing
            }

            if (verbose) cli::cli_progress_update(pb_id, 0.2)
            result_xcode <- xcode_cli_install(password = entered_password, verbose = describe_steps)

            if(!result_xcode) {
                cli::cli_abort(c(
                    "{.pkg macrtools}: Failed to install Xcode Command Line Tools.",
                    "This is a required component for R package development.",
                    "Installation status: Failed",
                    "i" = "Try installing manually by running 'sudo xcode-select --install' in Terminal."
                ))
            }

            if (verbose) cli::cli_progress_update(pb_id, 0.3)
        } else {
            if(describe_steps) {
                # Get Xcode CLI version information
                xcode_version <- base::tryCatch(
                    sys::as_text(sys::exec_internal('xcode-select', '--version')$stdout),
                    error = function(e) 'Unknown'
                )

                cli::cli_alert_info("{.pkg macrtools}: Xcode Command Line Tools already installed.")
                cli::cli_bullets(c(
                    "Location: {.path {xcode_cli_path()}}",
                    "Version: {.val {xcode_version}}",
                    "Status: Pre-installed, no action needed"
                ))
                cli::cli_text("") # Add spacing
            }
            if (verbose) cli::cli_progress_update(pb_id, 0.3)
        }
    } else {
        if(describe_steps) {
            # Get full Xcode app version information
            xcode_app_info <- base::tryCatch(
                sys::as_text(sys::exec_internal('xcodebuild', '-version')$stdout),
                error = function(e) "Unknown"
            )

            cli::cli_alert_info("{.pkg macrtools}: Full Xcode.app IDE is installed.")
            cli::cli_bullets(c(
                "Location: {.path {'/Applications/Xcode.app'}}",
                "Version information: {.val {xcode_app_info}}",
                "Status: Pre-installed, skipping Command Line Tools installation"
            ))
            cli::cli_text("") # Add spacing
        }
        if (verbose) cli::cli_progress_update(pb_id, 0.3)
    }

    # COMPONENT 2: GNU Fortran Compiler
    cli::cli_h3("Component 2 of 3: GNU Fortran Compiler")
    cli::cli_text("Essential compiler for scientific computing and many CRAN packages")
    cli::cli_ul(c(
        "Provides: Fortran compiler compatible with R's build tools",
        "Location: {gfortran_install_location()}/gfortran",
        "Size: ~1 GB"
    ))
    cli::cli_text("") # Add spacing

    # Step 2: gfortran
    if (verbose) cli::cli_progress_update(pb_id, 0.4)
    result_gfortran <- TRUE
    if(!is_gfortran_installed()) {
        if (verbose) {
            cli::cli_alert_info("{.pkg macrtools}: Need to install GNU Fortran compiler.")
            cli::cli_bullets(c(
                "Source: R-Project macOS tools repository",
                "Architecture: {.val {arch}}",
                "R version: {.val {r_version}}",
                "Status: Not installed",
                "Estimated time: 2-5 minutes"
            ))
            cli::cli_text("") # Add spacing
        }

        if (verbose) cli::cli_progress_update(pb_id, 0.5)
        result_gfortran <- gfortran_install(password = entered_password, verbose = describe_steps)
        if(!result_gfortran) {
            cli::cli_abort(c(
                "{.pkg macrtools}: Failed to install GNU Fortran compiler.",
                "This is a required component for many scientific R packages.",
                "Installation status: Failed",
                "i" = "Try installing manually following the instructions at: https://mac.thecoatlessprofessor.com/macrtools/reference/gfortran.html"
            ))
        }

        if (verbose) cli::cli_progress_update(pb_id, 0.6)
    } else {
        if(describe_steps) {
            # Get gfortran version information
            gfortran_version_info <- base::tryCatch(
                sys::as_text(sys::exec_internal('gfortran', '--version')$stdout),
                error = function(e) 'Unknown'
            )

            install_path <- base::file.path(gfortran_install_location(), 'gfortran')

            cli::cli_alert_info("{.pkg macrtools}: GNU Fortran compiler already installed.")
            cli::cli_bullets(c(
                "Location: {.path {install_path}}",
                "Version: {.val {gfortran_version_info}}",
                "Status: Pre-installed, no action needed"
            ))
            cli::cli_text("") # Add spacing
        }
        if (verbose) cli::cli_progress_update(pb_id, 0.6)
    }

    # COMPONENT 3: R Development Libraries
    cli::cli_h3("Component 3 of 3: R Development Libraries")
    cli::cli_text("Essential third-party libraries required for R package development")
    cli::cli_ul(c(
        "Provides: zlib, libbz2, liblzma, pcre2, and other dependencies",
        "Location: {recipe_binary_install_location(arch)}",
        "Size: ~2-3 GB"
    ))
    cli::cli_text("") # Add spacing

    # Step 3: r-base-dev
    if (verbose) {
        cli::cli_progress_update(pb_id, 0.7)
        cli::cli_alert_info("{.pkg macrtools}: Installing R development libraries.")
        cli::cli_bullets(c(
            "Source: R-Project macOS binary repository",
            "Target: {.path {recipe_binary_install_location(arch)}}",
            "Architecture: {.val {arch}}",
            "Status: Installation starting",
            "Estimated time: 5-10 minutes"
        ))
        cli::cli_text("") # Add spacing
    }

    result_base_dev <- recipes_binary_install(
        "r-base-dev", sudo = TRUE, password = entered_password, verbose = verbose
    )

    if (verbose) cli::cli_progress_update(pb_id, 0.9)

    # Finalize installation
    if (verbose) {
        cli::cli_progress_update(pb_id, 1.0)
        cli::cli_progress_done(pb_id)
    }

    rtools_install_clean <- result_gfortran && result_xcode && base::isTRUE(result_base_dev)

    if(rtools_install_clean) {

        cli::cli_h3("Installation Summary: Success")
        cli::cli_ul(c(
            "Xcode Command Line Tools installed",
            "GNU Fortran Compiler installed",
            "R Development Libraries installed"
        ))
        cli::cli_text("") # Add spacing

        cli::cli_alert_success("{.pkg macrtools}: macOS R development toolchain has been successfully installed!")
        cli::cli_text("") # Add spacing
        cli::cli_text("Your system is now configured for R package development.")
        cli::cli_text("You can install packages from source with: {.code install.packages('package_name', type = 'source')}")
        cli::cli_text("") # Add spacing

        current_time <- base::format(base::Sys.time(), '%Y-%m-%d %H:%M:%S')
        cli::cli_alert_info("Installation completed at: {.val {current_time}}")
        cli::cli_text("If you encounter issues with package installation, run:")
        cli::cli_code("pkgbuild::check_build_tools(debug = TRUE)")
    } else {
        cli::cli_abort(c(
            "{.pkg macrtools}: Installation failed. Some components could not be installed properly.",
            "Xcode CLI: {.val {if(result_xcode) 'Success' else 'Failed'}}",
            "Gfortran: {.val {if(result_gfortran) 'Success' else 'Failed'}}",
            "R development libraries: {.val {if(base::isTRUE(result_base_dev)) 'Success' else 'Failed'}}",
            "",
            "Please check the error messages above for more details.",
            "i" = "Try running the installation again or installing each component separately."
        ))
    }

    base::invisible(rtools_install_clean)
}


#' @rdname macos-rtools
#' @export
#' @examples
#'
#' \dontrun{
#'
#' # Remove Xcode CLI and gfortran tools required for compiling code on macOS with R
#' macos_rtools_uninstall()
#'
#' # Enter a password to avoid being prompted!
#' macos_rtools_uninstall(password = "hello-compiled-code-world")
#'
#' }
macos_rtools_uninstall <- function(
        password = base::getOption("macrtools.password"),
        verbose = TRUE
) {
    cli::cli_alert_info("{.pkg macrtools}: Beginning uninstallation process.")
    cli::cli_bullets(c(
        "Components to remove: Xcode CLI and gfortran",
        "Please ensure you are connected to a power source"
    ))
    cli::cli_text("") # Add spacing

    if(base::is.null(password)) {
        cli::cli_alert_info("{.pkg macrtools}: Administrative privileges required.")
        password <- askpass::askpass("Please enter your password to continue:")
    }

    # Create a progress bar
    if (verbose) {
        pb_id <- cli::cli_progress_bar(
            format = "{.pkg macrtools}: {.strong Uninstalling R development toolchain} {cli::pb_bar} {cli::pb_percent} {cli::pb_spin}",
            format_done = "{.pkg macrtools}: {.strong Uninstallation complete} {cli::symbol$tick}",
            total = 100
        )
    }

    # Step 1: Uninstall Xcode CLI
    result_xcode <- TRUE
    if(is_xcode_cli_installed()) {
        if (verbose) {
            cli::cli_progress_update(pb_id, 0.3)
            cli::cli_alert_info("{.pkg macrtools}: Uninstalling Xcode CLI...")
            cli::cli_text("") # Add spacing
        }
        result_xcode <- xcode_cli_uninstall(password = password, verbose = verbose)
        if(!result_xcode) {
            cli::cli_abort("{.pkg macrtools}: Failed to uninstall Xcode CLI. Please see manual steps.")
        }
    } else {
        if(verbose) {
            cli::cli_alert_info("{.pkg macrtools}: Xcode CLI was not installed, skipping uninstall procedure.")
            cli::cli_text("") # Add spacing
        }
    }

    # Step 2: Uninstall gfortran
    if (verbose) cli::cli_progress_update(pb_id, 0.7)
    result_gfortran <- TRUE
    if(is_gfortran_installed()) {
        if (verbose) {
            cli::cli_alert_info("{.pkg macrtools}: Uninstalling gfortran...")
            cli::cli_text("") # Add spacing
        }
        result_gfortran <- gfortran_uninstall(password = password, verbose = verbose)
        if(!result_gfortran) {
            cli::cli_abort("{.pkg macrtools}: Failed to uninstall gfortran. Please see manual steps.")
        }
    } else {
        if(verbose) {
            cli::cli_alert_info("{.pkg macrtools}: gfortran was not installed, skipping uninstall procedure.")
            cli::cli_text("") # Add spacing
        }
    }

    if (verbose) {
        cli::cli_progress_update(pb_id, 1.0)
        cli::cli_progress_done(pb_id)
    }

    clean <- result_gfortran && result_xcode

    if(clean) {
        cli::cli_alert_success("{.pkg macrtools}: Uninstallation complete.")
        cli::cli_bullets(c(
            "Xcode CLI: Successfully removed",
            "Gfortran: Successfully removed",
            "Note: This did not uninstall any binaries from the recipes project"
        ))
    }

    base::invisible(clean)
}
