#' Find, Install, or Uninstall OpenMP
#'
#' Set of functions that seek to identify whether OpenMP was installed,
#' allow OpenMP to be installed, and removing OpenMP.
#'
#' @details
#' The `openmp_*()` suite of functions attempts to locate, install, and uninstall
#' the OpenMP runtime library based on the installed Xcode version. These functions
#' mirror the logic described on the [macOS R Project OpenMP page](https://mac.r-project.org/openmp/)
#' and should be used by advanced users who wish to compile R packages
#' that use OpenMP for parallel processing. They are not required for most users.
#'
#' OpenMP runtime is downloaded from the R-project repository for macOS and installed to:
#'
#' - Library: `/usr/local/lib/libomp.dylib`
#' - Headers:
#'   - `/usr/local/include/omp.h`;
#'   - `/usr/local/include/ompt.h`;
#'   - `/usr/local/include/omp-tools.h`; and,
#'   - `/usr/local/include/ompx.h` (added in LLVM 19.1.0).
#'
#' **Note:** Apple has explicitly disabled OpenMP support in Xcode compilers, but the
#' runtime library can be installed separately and used with `-Xclang -fopenmp`
#' compiler flags.
#'
#' @section Check if `OpenMP` is installed:
#'
#' Checks the local file system for whether OpenMP runtime library is installed
#' in the default installation location.
#'
#' @rdname openmp
#' @export
#' @examples
#' # Check if OpenMP is installed
#' is_openmp_installed()
is_openmp_installed <- function() {
    assert_mac()

    # Check if the main library file exists
    library_path <- "/usr/local/lib/libomp.dylib"
    header_path <- "/usr/local/include/omp.h"

    # Both library and header should exist
    base::file.exists(library_path) && base::file.exists(header_path)
}

#' @export
#' @rdname openmp
openmp_version <- function() {
    if (!is_openmp_installed()) {
        cli::cli_abort("{.pkg macrtools}: OpenMP is not installed.")
    }

    # Try to get version information from the library
    library_path <- "/usr/local/lib/libomp.dylib"
    version_info <- base::tryCatch(
        sys::as_text(sys::exec_internal('otool', c('-L', library_path))$stdout),
        error = function(e) 'Unknown'
    )

    version_info
}

#' @section Installing `OpenMP`:
#' The `openmp_install()` function aims to install the appropriate OpenMP
#' runtime library based on the detected Xcode version.
#'
#' ### OpenMP Installation Process
#'
#' The installation process automatically detects your Xcode version and
#' downloads the corresponding OpenMP runtime from the R-project repository:
#'
#' ```sh
#' VERSION="19.1.0"
#'
#' # Download the appropriate version
#' curl -O https://mac.r-project.org/openmp/openmp-${VERSION}-darwin20-Release.tar.gz
#'
#' # Install to system directories
#' sudo tar fvxz openmp-${VERSION}-darwin20-Release.tar.gz -C /
#' ```
#'
#' ### Using OpenMP in R Packages
#'
#' During installation, we will automatically configure your `~/.R/Makevars`
#' file to include the necessary compiler flags for OpenMP support.
#'
#' ```sh
#' # macrtools - OpenMP: start
#' CPPFLAGS += -Xclang -fopenmp
#' LDFLAGS += -lomp
#' # macrtools - OpenMP: end
#' ```
#'
#' Alternatively, you can manually add the lines to your `~/.R/Makevars` file.
#' Or, install packages from command line with:
#'
#' ```sh
#' PKG_CPPFLAGS='-Xclang -fopenmp' PKG_LIBS=-lomp R CMD INSTALL myPackage
#' ```
#'
#' @param verbose  Display messages indicating status
#' @param password Password for user account to install software. Default is
#'                 `NULL`.
#'
#' @param configure_makevars Automatically configure ~/.R/Makevars with or without OpenMP flags.
#'                           Default is `TRUE`.
#' @export
#' @rdname openmp
openmp_install <- function(password = base::getOption("macrtools.password"), verbose = TRUE,
                           configure_makevars = TRUE) {
    assert_mac()
    assert_macos_supported()

    if(base::isTRUE(is_openmp_installed())) {
        if(verbose) {
            # Get OpenMP version info
            library_path <- "/usr/local/lib/libomp.dylib"
            version_info <- base::tryCatch(
                base::system(base::paste('otool -L', library_path, '| grep libomp'), intern = TRUE),
                error = function(e) 'Unknown'
            )

            cli::cli_alert_info("{.pkg macrtools}: OpenMP is already installed.")
            cli::cli_bullets(c(
                "Library path: {.path {library_path}}",
                "Version information: {.val {version_info}}"
            ))
            cli::cli_text("") # Add spacing
        }
        return(base::invisible(TRUE))
    }

    if (verbose) {
        # Get system info
        clang_info <- get_apple_clang_version()
        xcode_version <- clang_info$version_string

        cli::cli_alert_info("{.pkg macrtools}: Preparing to download and install OpenMP.")
        cli::cli_bullets(c(
            "Xcode version: {.val {xcode_version}}",
            "Expected installation time: 1-2 minutes on a broadband connection"
        ))
        cli::cli_text("") # Add spacing
    }

    # Prompt for password if not found
    entered_password_openmp <- force_password(password)

    # Determine the correct OpenMP version based on Xcode
    openmp_url <- get_openmp_url_for_xcode()

    if (base::is.null(openmp_url)) {
        clang_info <- get_apple_clang_version()
        xcode_version <- clang_info$version_string
        cli::cli_abort(c(
            "{.pkg macrtools}: Unable to determine compatible OpenMP version.",
            "Xcode version: {.val {xcode_version}}",
            "i" = "Please check the package documentation for manual installation instructions."
        ))
        return(base::invisible(FALSE))
    }

    if (verbose) {
        cli::cli_alert_info("{.pkg macrtools}: Installing OpenMP runtime library.")
        cli::cli_bullets(c(
            "Source: {.url {openmp_url}}",
            "Target installation: {.path /usr/local/}"
        ))
        cli::cli_text("") # Add spacing
    }

    # Download and install OpenMP
    openmp_status <- install_openmp_from_url(
        openmp_url,
        password = entered_password_openmp,
        verbose = verbose
    )

    if(base::isFALSE(openmp_status)) {
        cli::cli_abort(c(
            "{.pkg macrtools}: Failed to install OpenMP.",
            "i" = "Please check the package documentation for manual installation instructions."
        ))
        return(base::invisible(FALSE))
    }

    # Automatically configure Makevars if requested
    if (configure_makevars) {
        makevars_status <- configure_openmp_makevars(verbose = verbose)
        if (!makevars_status && verbose) {
            cli::cli_alert_warning("{.pkg macrtools}: OpenMP installed but Makevars configuration failed.")
            cli::cli_bullets(c(
                "You may need to manually add the following to ~/.R/Makevars:",
                "  CPPFLAGS += -Xclang -fopenmp",
                "  LDFLAGS += -lomp"
            ))
            cli::cli_text("") # Add spacing
        }
    } else if (verbose) {
        cli::cli_alert_info("{.pkg macrtools}: To use OpenMP, add the following to your ~/.R/Makevars:")
        cli::cli_bullets(c(
            "  CPPFLAGS += -Xclang -fopenmp",
            "  LDFLAGS += -lomp"
        ))
        cli::cli_text("") # Add spacing
    }

    return(base::invisible(TRUE))
}

#' @section Uninstalling `OpenMP`:
#'
#' The `openmp_uninstall()` attempts to remove OpenMP from the default
#' installation locations.
#'
#' We use the _R_ sanitized _shell_ command to remove the installed files:
#'
#' ```sh
#' sudo rm -f /usr/local/lib/libomp.dylib
#' sudo rm -f /usr/local/include/omp.h
#' sudo rm -f /usr/local/include/ompt.h
#' sudo rm -f /usr/local/include/omp-tools.h
#' sudo rm -f /usr/local/include/ompx.h
#' ```
#'
#' **Note:** `ompx.h` was included in LLVM 19.1.0 and
#' may not exist in older OpenMP versions.
#'
#' @export
#' @rdname openmp
openmp_uninstall <- function(password = base::getOption("macrtools.password"),
                             verbose = TRUE,
                             configure_makevars = TRUE) {
    assert_mac()

    if(base::isFALSE(is_openmp_installed())) {
        if(verbose) {
            cli::cli_alert_info("{.pkg macrtools}: OpenMP is not installed.")
            cli::cli_text("") # Add spacing
        }
        return(base::invisible(TRUE))
    }

    # Files to remove
    files_to_remove <- c(
        "/usr/local/lib/libomp.dylib",
        "/usr/local/include/omp.h",
        "/usr/local/include/ompt.h",
        "/usr/local/include/omp-tools.h",
        "/usr/local/include/ompx.h"  # May not exist in older versions
    )

    if (verbose) {
        cli::cli_alert_info("{.pkg macrtools}: Uninstalling OpenMP.")
        cli::cli_bullets(c(
            "Files to remove:",
            base::paste("  ", files_to_remove, collapse = "\n")
        ))
        cli::cli_text("") # Add spacing
    }

    # Remove files that exist
    existing_files <- files_to_remove[base::file.exists(files_to_remove)]

    if (base::length(existing_files) > 0) {
        cmd <- base::paste("rm -f", base::paste(existing_files, collapse = " "))

        openmp_uninstall_status <- shell_execute(
            cmd,
            sudo = TRUE,
            password = password,
            verbose = verbose
        )

        openmp_uninstall_clean <- base::identical(openmp_uninstall_status, 0L)

        if(base::isFALSE(openmp_uninstall_clean)) {
            cli::cli_abort(c(
                "{.pkg macrtools}: We were not able to uninstall OpenMP.",
                "i" = "Please check the package documentation for manual uninstall instructions."
            ))
            return(base::invisible(FALSE))
        }
    }

    if (verbose) {
        cli::cli_alert_success("{.pkg macrtools}: OpenMP was successfully uninstalled!")
        cli::cli_text("") # Add spacing
    }

    # Remove Makevars configuration if requested
    if (remove_makevars_config) {
        makevars_removal_status <- remove_openmp_makevars_config(verbose = verbose)
        if (!makevars_removal_status && verbose) {
            cli::cli_alert_warning("{.pkg macrtools}: OpenMP uninstalled but Makevars cleanup failed.")
            cli::cli_bullets(c(
                "You may need to manually remove OpenMP flags from ~/.R/Makevars"
            ))
            cli::cli_text("") # Add spacing
        }
    }

    return(base::invisible(TRUE))
}

# Helper functions ----

#' Get Apple Clang Version Information
#' @noRd
get_apple_clang_version <- function() {
    version_info <- base::tryCatch(
        sys::as_text(sys::exec_internal('clang', '--version')$stdout),
        error = function(e) NULL
    )

    if (base::is.null(version_info)) {
        return(list(version_string = "Unknown", build_number = 0))
    }

    # Extract Apple clang version string (format: "Apple clang version X.X.X")
    version_match <- base::regmatches(
        version_info,
        base::regexpr("Apple clang version [0-9]+\\.[0-9]+\\.[0-9]+", version_info)
    )

    version_string <- if (base::length(version_match) > 0) {
        base::regmatches(
            version_match,
            base::regexpr("[0-9]+\\.[0-9]+\\.[0-9]+", version_match)
        )
    } else {
        "Unknown"
    }

    # Look for clang-XXXX pattern which indicates Apple clang build number
    clang_match <- base::regmatches(
        version_info,
        base::regexpr("clang-[0-9]+", version_info)
    )

    build_number <- if (base::length(clang_match) > 0) {
        base::as.numeric(base::gsub("clang-", "", clang_match))
    } else {
        0
    }

    return(list(version_string = version_string, build_number = build_number))
}

#' Determine OpenMP URL based on Xcode version
#' @noRd
get_openmp_url_for_xcode <- function() {
    clang_info <- get_apple_clang_version()
    clang_version <- clang_info$build_number

    # Version mapping based on the documentation
    # Using the Release versions for stability
    openmp_mapping <- base::data.frame(
        min_clang = c(1700, 1600, 1500, 1403, 1400, 1316, 1300, 1205, 1200, 1103, 1100, 1001),
        filename = c(
            "openmp-19.1.0-darwin20-Release.tar.gz",  # Xcode 16.3+ (Apple clang 1700.x)
            "openmp-17.0.6-darwin20-Release.tar.gz",  # Xcode 16.0-16.2 (Apple clang 1600.x)
            "openmp-16.0.4-darwin20-Release.tar.gz",  # Xcode 15.x (Apple clang 1500.x)
            "openmp-15.0.7-darwin20-Release.tar.gz",  # Xcode 14.3.x (Apple clang 1403.x)
            "openmp-14.0.6-darwin20-Release.tar.gz",  # Xcode 14.0-14.2 (Apple clang 1400.x)
            "openmp-13.0.0-darwin21-Release.tar.gz",  # Xcode 13.3-13.4.1 (Apple clang 1316.x)
            "openmp-12.0.1-darwin20-Release.tar.gz",  # Xcode 13.0-13.2.1 (Apple clang 1300.x)
            "openmp-11.0.1-darwin20-Release.tar.gz",  # Xcode 12.5 (Apple clang 1205.x)
            "openmp-10.0.0-darwin17-Release.tar.gz",  # Xcode 12.0-12.4 (Apple clang 1200.x)
            "openmp-9.0.1-darwin17-Release.tar.gz",   # Xcode 11.4-11.7 (Apple clang 1103.x)
            "openmp-8.0.1-darwin17-Release.tar.gz",   # Xcode 11.0-11.3.1 (Apple clang 1100.x)
            "openmp-7.1.0-darwin17-Release.tar.gz"    # Xcode 10.2-10.3 (Apple clang 1001.x)
        ),
        stringsAsFactors = FALSE
    )

    # Find the appropriate version
    selected_file <- NULL
    for (i in base::seq_len(base::nrow(openmp_mapping))) {
        if (clang_version >= openmp_mapping$min_clang[i]) {
            selected_file <- openmp_mapping$filename[i]
            break
        }
    }

    if (base::is.null(selected_file)) {
        # Default to latest if version is too new or couldn't be detected
        selected_file <- "openmp-19.1.0-darwin20-Release.tar.gz"
    }

    base::paste0("https://mac.r-project.org/openmp/", selected_file)
}

#' Download and Install OpenMP from URL
#' @noRd
install_openmp_from_url <- function(url, password = NULL, verbose = TRUE) {

    # Download the tar file
    openmp_path <- binary_download(url, verbose = verbose)

    if (base::is.null(openmp_path) || !base::file.exists(openmp_path)) {
        return(FALSE)
    }

    # Install using tar directly to /
    if (verbose) {
        cli::cli_alert_info("{.pkg macrtools}: Installing OpenMP files to system directories.")
        cli::cli_text("") # Add spacing
    }

    cmd <- base::paste("tar fvxz", openmp_path, "-C /")
    install_status <- shell_execute(
        cmd,
        sudo = TRUE,
        password = password,
        verbose = verbose
    )

    # Clean up downloaded file
    base::unlink(openmp_path)

    if (install_status != 0) {
        return(FALSE)
    }

    # Verify installation
    if (!is_openmp_installed()) {
        return(FALSE)
    }

    return(TRUE)
}

#' @section Testing OpenMP Installation:
#' After installing OpenMP, you can test if it's working by creating a simple
#' test package or by adding the following to your `~/.R/Makevars` file:
#'
#' ```sh
#' CPPFLAGS += -Xclang -fopenmp
#' LDFLAGS += -lomp
#' ```
#'
#' Then try installing a package that uses OpenMP, such as:
#'
#' ```r
#' install.packages("data.table", type = "source")
#' ```
#'
#' @export
#' @rdname openmp
openmp_test <- function() {
    if (!is_openmp_installed()) {
        cli::cli_abort("{.pkg macrtools}: OpenMP is not installed. Run openmp_install() first.")
    }

    # Check if Makevars has OpenMP flags
    makevars_path <- "~/.R/Makevars"

    if (base::file.exists(base::path.expand(makevars_path))) {
        makevars_content <- base::readLines(base::path.expand(makevars_path))
        has_openmp_cpp <- base::any(base::grepl("-Xclang -fopenmp", makevars_content))
        has_openmp_ld <- base::any(base::grepl("-lomp", makevars_content))

        if (has_openmp_cpp && has_openmp_ld) {
            cli::cli_alert_success("{.pkg macrtools}: OpenMP is properly configured in ~/.R/Makevars")
        } else {
            cli::cli_alert_warning("{.pkg macrtools}: OpenMP flags not found in ~/.R/Makevars")
            cli::cli_bullets(c(
                "Add the following lines to ~/.R/Makevars:",
                "  CPPFLAGS += -Xclang -fopenmp",
                "  LDFLAGS += -lomp"
            ))
        }
    } else {
        cli::cli_alert_info("{.pkg macrtools}: ~/.R/Makevars not found.")
        cli::cli_bullets(c(
            "Create ~/.R/Makevars with the following content:",
            "  CPPFLAGS += -Xclang -fopenmp",
            "  LDFLAGS += -lomp"
        ))
    }

    # Test library signature
    library_path <- "/usr/local/lib/libomp.dylib"
    signature_info <- base::tryCatch(
        sys::as_text(sys::exec_internal('codesign', c('-d', '-vv', library_path))$stderr),
        error = function(e) "Could not verify signature"
    )

    cli::cli_alert_info("{.pkg macrtools}: OpenMP library signature verification:")
    cli::cli_text(signature_info)

    return(base::invisible(TRUE))
}


#' Configure OpenMP flags in ~/.R/Makevars
#' @param verbose Display status messages
#' @noRd
configure_openmp_makevars <- function(verbose = TRUE) {
    makevars_path <- base::path.expand("~/.R/Makevars")

    # Create ~/.R directory if it doesn't exist
    r_dir <- base::path.expand("~/.R")
    if (!base::dir.exists(r_dir)) {
        base::tryCatch(
            base::dir.create(r_dir, recursive = TRUE),
            error = function(e) {
                if (verbose) {
                    cli::cli_alert_warning("{.pkg macrtools}: Could not create ~/.R directory: {.val {e$message}}")
                }
                return(FALSE)
            }
        )
    }

    if (verbose) {
        cli::cli_alert_info("{.pkg macrtools}: Configuring OpenMP flags in ~/.R/Makevars")
        if (base::file.exists(makevars_path)) {
            cli::cli_bullets(c(
                "Existing Makevars file found: {.path {makevars_path}}"
            ))
        } else {
            cli::cli_bullets(c(
                "Creating new Makevars file: {.path {makevars_path}}"
            ))
        }
        cli::cli_text("") # Add spacing
    }

    # Define the OpenMP configuration
    openmp_config <- c(
        "CPPFLAGS += -Xclang -fopenmp",
        "LDFLAGS += -lomp"
    )

    # Use the block system to add configuration
    config_result <- base::tryCatch({
        block_append(
            desc = "OpenMP configuration",
            value = openmp_config,
            path = makevars_path,
            block_start = "# macrtools - OpenMP: start",
            block_end = "# macrtools - OpenMP: end"
        )
        TRUE
    }, error = function(e) {
        if (verbose) {
            cli::cli_alert_warning("{.pkg macrtools}: Failed to configure Makevars: {.val {e$message}}")
        }
        FALSE
    })

    if (config_result && verbose) {
        cli::cli_alert_success("{.pkg macrtools}: OpenMP flags added to ~/.R/Makevars")
        cli::cli_bullets(c(
            "CPPFLAGS += -Xclang -fopenmp",
            "LDFLAGS += -lomp",
            "You may need to restart R for the changes to take effect."
        ))
        cli::cli_text("") # Add spacing
    }

    return(config_result)
}


#' Remove OpenMP configuration from ~/.R/Makevars
#' @param verbose Display status messages
#' @noRd
remove_openmp_makevars_config <- function(verbose = TRUE) {
    makevars_path <- base::path.expand("~/.R/Makevars")

    if (!base::file.exists(makevars_path)) {
        if (verbose) {
            cli::cli_alert_info("{.pkg macrtools}: No ~/.R/Makevars file found, nothing to remove.")
            cli::cli_text("") # Add spacing
        }
        return(TRUE)
    }

    if (verbose) {
        cli::cli_alert_info("{.pkg macrtools}: Removing OpenMP configuration from ~/.R/Makevars")
        cli::cli_text("") # Add spacing
    }

    # Read the current file
    current_lines <- base::tryCatch(
        read_utf8(makevars_path),
        error = function(e) {
            if (verbose) {
                cli::cli_alert_warning("{.pkg macrtools}: Could not read Makevars file: {.val {e$message}}")
            }
            return(NULL)
        }
    )

    if (base::is.null(current_lines)) {
        return(FALSE)
    }

    # Find and remove the OpenMP block
    block_lines <- block_find(
        current_lines,
        block_start = "# macrtools - OpenMP: start",
        block_end = "# macrtools - OpenMP: end"
    )

    if (base::is.null(block_lines)) {
        if (verbose) {
            cli::cli_alert_info("{.pkg macrtools}: No OpenMP configuration block found in ~/.R/Makevars")
            cli::cli_text("") # Add spacing
        }
        return(TRUE)
    }

    # Remove the block (including start and end markers)
    start_line <- block_lines[[1]] - 1  # Include the start marker
    end_line <- block_lines[[2]] + 1    # Include the end marker

    # Create new content without the OpenMP block
    new_lines <- base::c(
        if (start_line > 1) current_lines[1:(start_line - 1)] else character(0),
        if (end_line < base::length(current_lines)) current_lines[(end_line + 1):base::length(current_lines)] else character(0)
    )

    # Write the updated file
    removal_result <- base::tryCatch({
        write_utf8(makevars_path, new_lines)
        TRUE
    }, error = function(e) {
        if (verbose) {
            cli::cli_alert_warning("{.pkg macrtools}: Failed to update Makevars file: {.val {e$message}}")
        }
        FALSE
    })

    if (removal_result && verbose) {
        cli::cli_alert_success("{.pkg macrtools}: OpenMP configuration removed from ~/.R/Makevars")
        cli::cli_text("") # Add spacing
    }

    return(removal_result)
}
