#' @include xcode-cli.R gfortran.R recipes.R cli-custom.R
NULL

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
        password = getOption("macrtools.password"),
        verbose = TRUE
) {

    # Initial system check
    assert_mac()
    assert_macos_supported()
    assert_r_version_supported()

    cli_system_update("Installing macOS R Development Toolchain", c(
        "This process will install all components required for R package development on macOS:",
        "Xcode Command Line Tools - Apple's development utilities",
        "gfortran - GNU Fortran compiler required for many scientific packages",
        "r-base-dev - Essential libraries from the R-macOS Recipes project"
    ))

    cli_info(c(
        "System requirements check:",
        "Operating system: {.val {shell_mac_version()}} ({.val {Sys.info()['release']}})",
        "Architecture: {.val {system_arch()}}",
        "R version: {.val {paste(R.version$major, R.version$minor, sep='.')}}",
        "Available disk space: {.val {round(as.numeric(system('df -h / | tail -1 | awk \'{print $4}\'', intern=TRUE)), 2)}} GB",
        "Administrator privileges: Required"
    ))

    cli_info(c(
        "Installation prerequisites:",
        "Ensure you have a stable internet connection",
        "Connect your computer to a power source",
        "Estimated installation time: 15-25 minutes",
        "Required disk space: Approximately 5-7 GB"
    ))

    entered_password <- password
    if(base::is.null(entered_password)) {
        cli_info("Administrative privileges are required for installation.")
        entered_password <- askpass::askpass("Please enter your administrator password:")
    }

    describe_steps <- isTRUE(verbose)

    # Create a detailed progress bar
    if (verbose) {
        pb_id <- cli_process_start("Installing R development toolchain", total = 100)
        cli_info("Installation process started at: {.val {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}}")
    }

    # COMPONENT 1: Xcode Command Line Tools
    cli_system_update("Component 1 of 3: Xcode Command Line Tools", c(
        "Apple's development utilities providing compilers and build tools",
        "Includes: gcc, make, git, clang, and other essential development tools",
        "Location: /Library/Developer/CommandLineTools",
        "Size: ~1.5 GB"
    ))

    # Step 1: Xcode CLI
    result_xcode <- TRUE
    if(!is_xcode_app_installed()) {
        if(!is_xcode_cli_installed()) {
            if (verbose) {
                cli_process_update(pb_id, 0.1, "Checking Xcode CLI requirements")
                cli_info(c(
                    "Need to install Xcode Command Line Tools.",
                    "Source: Apple Software Update",
                    "Installation method: softwareupdate command",
                    "Status: Not installed",
                    "Estimated time: 5-10 minutes"
                ))
            }

            if (verbose) cli_process_update(pb_id, 0.2, "Installing Xcode CLI")
            result_xcode <- xcode_cli_install(password = entered_password, verbose = describe_steps)

            if(!result_xcode) {
                cli_error(c(
                    "Failed to install Xcode Command Line Tools.",
                    "This is a required component for R package development.",
                    "Installation status: Failed"
                ),
                advice = "Try installing manually by running 'xcode-select --install' in Terminal.")
            }

            if (verbose) cli_process_update(pb_id, 0.3, "Xcode CLI installation complete")
        } else {
            if(describe_steps) {
                cli_info(c(
                    "Xcode Command Line Tools already installed.",
                    "Location: {.path {xcode_cli_path()}}",
                    "Version: {.val {tryCatch(sys::as_text(sys::exec_internal('xcode-select', '--version')$stdout), error = function(e) 'Unknown')}}",
                    "Status: Pre-installed, no action needed"
                ))
            }
            if (verbose) cli_process_update(pb_id, 0.3, "Xcode CLI already installed")
        }
    } else {
        if(describe_steps) {
            xcode_app_info <- tryCatch(sys::as_text(sys::exec_internal('xcodebuild', '-version')$stdout), error = function(e) "Unknown")
            cli_info(c(
                "Full Xcode.app IDE is installed.",
                "Location: {.path {'/Applications/Xcode.app'}}",
                "Version information: {.val {xcode_app_info}}",
                "Status: Pre-installed, skipping Command Line Tools installation"
            ))
        }
        if (verbose) cli_process_update(pb_id, 0.3, "Using existing Xcode.app")
    }

    # COMPONENT 2: GNU Fortran Compiler
    cli_system_update("Component 2 of 3: GNU Fortran Compiler", c(
        "Essential compiler for scientific computing and many CRAN packages",
        "Provides: Fortran compiler compatible with R's build tools",
        paste0("Location: ", gfortran_install_location(), "/gfortran"),
        "Size: ~1 GB"
    ))

    # Step 2: gfortran
    if (verbose) cli_process_update(pb_id, 0.4, "Checking gfortran requirements")
    result_gfortran <- TRUE
    if(!is_gfortran_installed()) {
        if (verbose) {
            cli_info(c(
                "Need to install GNU Fortran compiler.",
                "Source: R-Project macOS tools repository",
                "Architecture: {.val {system_arch()}}",
                "R version: {.val {paste(R.version$major, R.version$minor, sep='.')}}",
                "Status: Not installed",
                "Estimated time: 2-5 minutes"
            ))
        }

        if (verbose) cli_process_update(pb_id, 0.5, "Installing gfortran")
        result_gfortran <- gfortran_install(password = entered_password, verbose = describe_steps)
        if(!result_gfortran) {
            cli_error(c(
                "Failed to install GNU Fortran compiler.",
                "This is a required component for many scientific R packages.",
                "Installation status: Failed"
            ),
            advice = "Try installing manually following the instructions at: https://mac.thecoatlessprofessor.com/macrtools/reference/gfortran.html")
        }

        if (verbose) cli_process_update(pb_id, 0.6, "Gfortran installation complete")
    } else {
        if(describe_steps) {
            cli_info(c(
                "GNU Fortran compiler already installed.",
                "Location: {.path {file.path(gfortran_install_location(), 'gfortran')}}",
                "Version: {.val {tryCatch(sys::as_text(sys::exec_internal('gfortran', '--version')$stdout), error = function(e) 'Unknown')}}",
                "Status: Pre-installed, no action needed"
            ))
        }
        if (verbose) cli_process_update(pb_id, 0.6, "Gfortran already installed")
    }

    # COMPONENT 3: R Development Libraries
    cli_system_update("Component 3 of 3: R Development Libraries", c(
        "Essential third-party libraries required for R package development",
        "Provides: zlib, libbz2, liblzma, pcre2, and other dependencies",
        paste0("Location: ", recipe_binary_install_location(system_arch())),
        "Size: ~2-3 GB"
    ))

    # Step 3: r-base-dev
    if (verbose) {
        cli_process_update(pb_id, 0.7, "Installing R development libraries")
        cli_info(c(
            "Installing R development libraries from the R-macOS recipes project.",
            "Source: R-Project macOS binary repository",
            "Target: {.path {recipe_binary_install_location(system_arch())}}",
            "Architecture: {.val {system_arch()}}",
            "Status: Installation starting",
            "Estimated time: 5-10 minutes"
        ))
    }

    result_base_dev <- recipes_binary_install(
        "r-base-dev", sudo = TRUE, password = entered_password, verbose = verbose
    )

    if (verbose) cli_process_update(pb_id, 0.9, "Development libraries installation complete")

    # Finalize installation
    if (verbose) {
        cli_process_update(pb_id, 1.0, "Installation complete")
        cli_process_done(pb_id)
    }

    rtools_install_clean <- result_gfortran && result_xcode && base::isTRUE(result_base_dev)

    if(rtools_install_clean) {
        # Generate system report
        r_version <- base::paste(R.version$major, R.version$minor, sep='.')
        xcode_info <- base::tryCatch(sys::as_text(sys::exec_internal('xcode-select', '--version')$stdout), error = function(e) "Unknown")
        gfortran_info <- base::tryCatch(sys::as_text(sys::exec_internal('gfortran', '--version')$stdout), error = function(e) "Unknown")

        cli_system_update("Installation Summary: Success", c(
            "Xcode Command Line Tools installed",
            "GNU Fortran Compiler installed",
            "R Development Libraries installed"
        ))

        cli_success(c(
            "macOS R development toolchain has been successfully installed!",
            "",
            "System configuration:",
            "macOS version: {.val {shell_mac_version()}}",
            "Architecture: {.val {system_arch()}}",
            "R version: {.val {r_version}}",
            "Xcode tools: {.val {base::substr(xcode_info, 1, 20)}}...",
            "Fortran: {.val {base::substr(gfortran_info, 1, 20)}}...",
            "",
            "Your system is now configured for R package development.",
            "You can install packages from source with: {.code install.packages('package_name', type = 'source')}"
        ))

        cli_info(c(
            "Installation completed at: {.val {format(Sys.time(), '%Y-%m-%d %H:%M:%S')}}",
            "If you encounter issues with package installation, run:",
            "{.code pkgbuild::check_build_tools(debug = TRUE)}"
        ))
    } else {
        cli_error(c(
            "Installation failed. Some components could not be installed properly.",
            "Xcode CLI: {.val {if(result_xcode) 'Success' else 'Failed'}}",
            "Gfortran: {.val {if(result_gfortran) 'Success' else 'Failed'}}",
            "R development libraries: {.val {if(base::isTRUE(result_base_dev)) 'Success' else 'Failed'}}",
            "",
            "Please check the error messages above for more details."
        ),
        advice = "Try running the installation again or installing each component separately.")
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
        password = getOption("macrtools.password"),
        verbose = TRUE
) {
    cli_info(c(
        "Uninstalling Xcode CLI and gfortran...",
        "Please ensure you are connected to a power source."
    ))

    if(is.null(password)) {
        cli_info("Please enter your password to continue.")
        password <- askpass::askpass()
    }

    # Create a progress bar
    if (verbose) {
        pb_id <- cli_process_start("Uninstalling R development toolchain")
    }

    # Step 1: Uninstall Xcode CLI
    result_xcode <- TRUE
    if(is_xcode_cli_installed()) {
        if (verbose) {
            cli_process_update(pb_id, 0.3)
            cli_info("Uninstalling Xcode CLI...")
        }
        result_xcode <- xcode_cli_uninstall(password = password, verbose = verbose)
        if(!result_xcode) {
            cli_error("Failed to uninstall Xcode CLI. Please see manual steps.")
        }
    } else {
        if(verbose) cli_info("Xcode CLI was not installed, skipping uninstall procedure.")
    }

    # Step 2: Uninstall gfortran
    if (verbose) cli_process_update(pb_id, 0.7)
    result_gfortran <- TRUE
    if(is_gfortran_installed()) {
        if (verbose) cli_info("Uninstalling gfortran...")
        result_gfortran <- gfortran_uninstall(password = password, verbose = verbose)
        if(!result_gfortran) {
            cli_error("Failed to uninstall gfortran. Please see manual steps.")
        }
    } else {
        if(verbose) cli_info("gfortran was not installed, skipping uninstall procedure.")
    }

    if (verbose) {
        cli_process_update(pb_id, 1.0)
        cli_process_done(pb_id)
    }

    clean <- result_gfortran && result_xcode

    if(clean) {
        cli_success(c(
            "Xcode CLI and Gfortran have been successfully removed from your system.",
            "Note: This did not uninstall any binaries from the recipes project."
        ))
    }

    invisible(clean)
}
