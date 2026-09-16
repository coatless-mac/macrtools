#' Interface with `xcode-select` Shell Commands
#'
#' Trigger `xcode-select` commands from within _R_
#'
#' @param args Flag arguments to pass to `xcode-select`
#' @export
#' @rdname xcode-select
xcode_select <- function(args) {
    out <- sys::exec_internal("xcode-select", args = args, error = FALSE)

    base::structure(
        base::list(
            output = sys::as_text(out$stdout),
            error = sys::as_text(out$stderr),
            status = out$status
        ),
        class = c("xcodeselect", "cli")
    )
}

#' @export
#' @rdname xcode-select
xcode_select_path <- function() {
    xcode_select("--print-path")
}

#' @export
#' @rdname xcode-select
xcode_select_version <- function() {
    xcode_select("--version")
}

#' Interface with `xcodebuild` Shell Commands
#'
#' Trigger `xcodebuild` commands from within _R_
#'
#' @param args Flag arguments to pass to `xcodebuild`
#' @export
#' @rdname xcodebuild
xcodebuild <- function(args) {
    out <- sys::exec_internal("xcodebuild", args = args, error = FALSE)

    base::structure(
        base::list(
            output = sys::as_text(out$stdout),
            error = sys::as_text(out$stderr),
            status = out$status
        ),
        class = c("xcodebuild", "cli")
    )
}

#' @export
#' @rdname xcodebuild
xcodebuild_version <- function() {
    xcodebuild("-version")
}

#' Detect if the Xcode.app IDE is Installed
#'
#' Checks whether an Xcode.app Integrated Developer Environment (IDE) is
#' installed anywhere on the system.
#'
#' @details
#' Installed, not *selected*. Xcode is routinely installed under a versioned
#' name such as `/Applications/Xcode_26.6.app`, which is what every
#' GitHub-hosted macOS runner does, and `man xcode-select` documents
#' non-default locations as supported. Bundles are therefore located by bundle
#' identifier rather than by path, so a renamed or relocated install is still
#' found.
#'
#' To ask instead which toolchain is *active*, use [developer_dir()]; to ask
#' whether a compiler can actually be reached, use
#' [is_xcode_toolchain_usable()].
#'
#' @rdname xcode-app-ide
#' @export
#' @examples
#' # Check if Xcode.app IDE is on the path
#' is_xcode_app_installed()
is_xcode_app_installed <- function() {
    assert_mac()

    base::length(xcode_app_bundles()) > 0L
}

#' Normalize a Developer Directory Path
#'
#' @details
#' `xcode-select --switch` accepts either an `Xcode.app` bundle or a developer
#' directory inside one, so both spellings are normalized to the developer
#' directory. Trailing slashes and symlinks are resolved too, because macOS
#' reports paths in forms that [base::identical()] will never match.
#'
#' @param path A bundle path or developer directory.
#' @return The normalized developer directory.
#' @keywords internal
normalize_developer_dir <- function(path) {
    path <- base::sub("/+$", "", path)

    is_bundle <- base::grepl("\\.app$", path)
    path[is_bundle] <- base::file.path(path[is_bundle], "Contents", "Developer")

    base::normalizePath(path, mustWork = FALSE)
}

#' Inspect the Active Xcode Toolchain
#'
#' Report which developer directory is active, whether a compiler can actually
#' be reached through it, and which toolchains are available to switch to.
#'
#' @details
#' `developer_dir()` resolves the active developer directory.
#' `DEVELOPER_DIR` overrides the selection stored by `xcode-select`, for
#' `xcrun`, the `/usr/bin` shims and `xcodebuild` alike, so it is consulted
#' first. Note that `xcode-select --print-path` echoes its value and exits `0`
#' even when the directory does not exist, so the exit status alone proves
#' nothing; the path is verified before being returned.
#'
#' @return The active developer directory, or `NA_character_` when nothing
#'   usable is selected.
#' @name xcode-toolchain
#' @rdname xcode-toolchain
#' @export
#' @examples
#' # Which developer directory is active
#' developer_dir()
developer_dir <- function() {
    from_env <- base::Sys.getenv("DEVELOPER_DIR", unset = "")

    candidate <- if (base::nzchar(from_env)) {
        from_env
    } else {
        # xcode-select may be missing entirely, so failing to run it must
        # resolve to "nothing selected" rather than propagating an error.
        path_info <- base::tryCatch(xcode_select_path(), error = function(e) NULL)
        if (!base::is.null(path_info) && base::identical(path_info$status, 0L)) {
            path_info$output
        } else {
            ""
        }
    }

    if (base::length(candidate) != 1L || base::is.na(candidate) ||
        !base::nzchar(candidate)) {
        return(NA_character_)
    }

    candidate <- normalize_developer_dir(candidate)

    if (!base::dir.exists(candidate)) NA_character_ else candidate
}

#' Locate Every Installed Xcode Bundle
#'
#' @details
#' Xcode is routinely installed under a versioned name, which is what every
#' GitHub-hosted macOS runner does (`/Applications/Xcode_26.6.app`), and
#' `man xcode-select` documents non-default locations as supported. Spotlight
#' is queried by bundle identifier so the name and location do not matter, with
#' a glob fallback for machines where Spotlight indexing is disabled. Time
#' Machine copies are discarded.
#'
#' @return Paths to the installed Xcode bundles, possibly empty.
#' @keywords internal
xcode_app_bundles <- function() {
    from_spotlight <- exec_text(
        "mdfind",
        "kMDItemCFBundleIdentifier == 'com.apple.dt.Xcode'",
        fallback = base::character(0)
    )

    candidates <- base::unique(c(
        from_spotlight,
        base::Sys.glob("/Applications/Xcode*.app")
    ))

    candidates <- candidates[!base::is.na(candidates) & base::nzchar(candidates)]
    candidates <- candidates[
        !base::grepl("/Backups.backupdb/", candidates, fixed = TRUE)
    ]
    candidates <- candidates[
        base::dir.exists(base::file.path(candidates, "Contents", "Developer"))
    ]

    base::unique(base::normalizePath(candidates, mustWork = FALSE))
}

#' Version of an Xcode Bundle
#'
#' @param bundle Path to an `Xcode.app` bundle.
#' @return The bundle's short version string, or `NA_character_`.
#' @keywords internal
xcode_app_version <- function(bundle) {
    plist <- base::file.path(bundle, "Contents", "version.plist")

    if (!base::file.exists(plist)) {
        return(NA_character_)
    }

    out <- exec_text(
        "/usr/libexec/PlistBuddy",
        c("-c", "Print :CFBundleShortVersionString", plist),
        fallback = NA_character_
    )

    if (base::length(out) != 1L || base::is.na(out) || !base::nzchar(out)) {
        NA_character_
    } else {
        out
    }
}

#' Check if a Developer Directory is a Full Xcode
#'
#' @details
#' A full Xcode carries the platform bundles; the Command Line Tools do not.
#' This distinguishes the two without shelling out to `xcodebuild`, which is
#' slow, errors when the Command Line Tools are active, and can stall on the
#' license agreement.
#'
#' @param dir Developer directory to test, defaulting to the active one.
#' @return TRUE if `dir` is a full Xcode developer directory, FALSE otherwise
#' @keywords internal
developer_dir_is_app <- function(dir = developer_dir()) {
    !base::is.na(dir) &&
        base::dir.exists(base::file.path(dir, "Platforms", "MacOSX.platform"))
}

#' Check if the Active Toolchain Can Actually Compile
#'
#' @details
#' This is the question that matters for building R packages. R is configured
#' with a bare `clang`, which resolves to the `/usr/bin` shim, so whether a
#' package can be compiled depends on the active developer directory
#' dispatching successfully, not on where the tools live.
#'
#' `xcrun --find clang` resolves through the active directory and honors
#' `DEVELOPER_DIR`. It can, however, exit `0` while printing an error for a
#' degraded bundle, so the returned path is confirmed to exist and be
#' executable rather than trusting the exit status.
#'
#' @return TRUE if a usable compiler is reachable, FALSE otherwise
#' @rdname xcode-toolchain
#' @export
#' @examples
#' # Check whether a compiler is actually reachable
#' is_xcode_toolchain_usable()
is_xcode_toolchain_usable <- function() {
    assert_mac()

    found <- exec_text("xcrun", c("--find", "clang"), fallback = NA_character_)

    if (base::length(found) != 1L || base::is.na(found) || !base::nzchar(found)) {
        return(FALSE)
    }

    base::file.exists(found) && base::identical(base::file.access(found, 1L)[[1]], 0L)
}

#' Find, Install, or Uninstall XCode CLI
#'
#' Set of functions that seek to identify whether XCode CLI was installed,
#' allow XCode CLI to be installed, and removing XCode CLI.
#'
#' @section Check if XCode CLI is installed:
#'
#' `is_xcode_cli_installed()` reports whether the Command Line Tools *package*
#' is present, by looking for the compiler it installs:
#'
#' ```sh
#' /Library/Developer/CommandLineTools/usr/bin/clang
#' ```
#'
#' This is independent of which developer directory is selected. The previous
#' implementation compared `xcode-select -p` against that path with
#' `identical()`, so it reported `FALSE` on every machine where anything else
#' was active, including the versioned Xcode that CI runners select.
#'
#' For the selection, see [developer_dir()] and [is_xcode_toolchain_usable()].
#'
#' @rdname xcode-cli
#' @export
#' @examples
#' # Check if Xcode CLI is installed
#' is_xcode_cli_installed()
is_xcode_cli_installed <- function() {
    assert_mac()

    base::file.exists(
        base::file.path(install_directory_xcode_cli(), "usr", "bin", "clang")
    )
}

#' @return The Command Line Tools version from the package receipt, or
#'   `NA_character_` when they are not installed.
#' @rdname xcode-cli
#' @export
#' @examples
#' # Version of the installed Command Line Tools
#' xcode_cli_version()
xcode_cli_version <- function() {
    if (!is_xcode_cli_installed()) {
        return(NA_character_)
    }

    info <- exec_text(
        "pkgutil",
        "--pkg-info=com.apple.pkg.CLTools_Executables",
        fallback = NA_character_
    )

    version_line <- base::grep("^version:", info, value = TRUE)

    if (base::length(version_line) == 0L) {
        return(NA_character_)
    }

    base::trimws(base::sub("^version:", "", version_line[[1]]))
}

#' @section Listing the Available Toolchains:
#'
#' `xcode_toolchains()` reports every developer directory `xcode-select` could
#' be pointed at: the Command Line Tools, plus each installed Xcode bundle
#' wherever it lives. Pass a `path` from this table to [xcode_cli_switch()].
#'
#' Note that this lists *developer directories*, not `.xctoolchain` bundles.
#' Those are a separate axis selected through `xcrun --toolchain` and the
#' `TOOLCHAINS` variable, and are mostly Swift snapshots.
#'
#' @return A data frame with one row per selectable developer directory and
#'   columns `path`, `type` (`"cli"` or `"app"`), `version` and `active`.
#' @rdname xcode-toolchain
#' @export
#' @examples
#' # List every toolchain xcode-select could use
#' xcode_toolchains()
xcode_toolchains <- function() {
    assert_mac()

    empty <- base::data.frame(
        path = base::character(0),
        type = base::character(0),
        version = base::character(0),
        active = base::logical(0),
        stringsAsFactors = FALSE
    )

    rows <- base::list()

    if (is_xcode_cli_installed()) {
        rows[[base::length(rows) + 1L]] <- base::data.frame(
            path = install_directory_xcode_cli(),
            type = "cli",
            version = xcode_cli_version(),
            stringsAsFactors = FALSE
        )
    }

    for (bundle in xcode_app_bundles()) {
        rows[[base::length(rows) + 1L]] <- base::data.frame(
            path = base::file.path(bundle, "Contents", "Developer"),
            type = "app",
            version = xcode_app_version(bundle),
            stringsAsFactors = FALSE
        )
    }

    if (base::length(rows) == 0L) {
        return(empty)
    }

    out <- base::do.call(base::rbind, rows)

    # developer_dir() is normalized, so normalize this side too rather than
    # relying on the literals happening to already be in canonical form.
    active <- developer_dir()
    out$active <- !base::is.na(active) &
        normalize_developer_dir(out$path) == active

    out <- out[base::order(out$type, out$path), , drop = FALSE]
    base::row.names(out) <- NULL
    out
}

#' @rdname xcode-cli
#' @export
#' @examples
#' # Determine the path location of Xcode CLI
#' xcode_cli_path()
xcode_cli_path <- function() {
    inquiry_on_path <- xcode_select_path()
    if (base::identical(inquiry_on_path$status, 0L)) {
        inquiry_on_path$output
    } else {
        ""
    }
}

#' @section XCode CLI Installation:
#'
#' The `xcode_cli_install()` function performs a headless or non-interactive
#' installation of the Xcode CLI tools. This installation process requires
#' three steps:
#'
#' 1. Place a temporary file indicating the need to download Xcode CLI
#' 2. Determine the latest version of Xcode CLI by running `softwareupdate`
#' 3. Install the latest version using `softwareupdate` with `sudo`.
#'
#' The alternative approach would be an interactive installation of Xcode CLI
#' by typing into Terminal:
#'
#' ```sh
#' sudo xcode-select --install
#' ```
#'
#' This command will trigger a pop up window that will walk through the
#' package installation.
#'
#' ### Steps of the Headless CLI Installation
#'
#' The temporary file is created using:
#'
#' ```sh
#' touch /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress
#' ```
#'
#' From there, we deduce the latest version of Xcode available to the user
#' through an _R_ sanitized version of the chained _shell_ commands:
#'
#' ```sh
#' product_information=softwareupdate -l |
#'    grep '\\*.*Command Line' |
#'    tail -n 1 |
#'    awk -F"*" '{print $2}' |
#'    sed -e 's/^ *//' |
#'    sed 's/Label: //g' |
#'    tr -d '\n'
#' ```
#'
#' Then, we trigger the installation process with `sudo` using:
#'
#' ```sh
#' sudo softwareupdate -i "$product_information" --verbose
#' ```
#'
#' where `$product_information` is obtained from the previous command.
#'
#' Finally, we remove the temporary installation file.
#'
#' ```sh
#' rm /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress
#' ```
#'
#' These steps were obtained from Timothy Sutton's
#' [xcode-cli-tools.sh](https://github.com/timsutton/osx-vm-templates/blob/ce8df8a7468faa7c5312444ece1b977c1b2f77a4/scripts/xcode-cli-tools.sh#L8-L14)
#' script and, slightly modernized.
#'
#' @export
#' @rdname xcode-cli
#' @param verbose    Display status messages
#' @param password   User password to access `sudo`.
xcode_cli_install <- function(password = base::getOption("macrtools.password"), verbose = TRUE){
    assert_mac()

    if (base::isTRUE(is_xcode_cli_installed())) {
        if(verbose) {
            cli::cli_alert_info("{.pkg macrtools}: Xcode CLI is already installed.")
            cli::cli_text("") # Add spacing
        }
        return(base::invisible(TRUE))
    }

    # Skipping requires a toolchain that is actually ACTIVE. An Xcode sitting
    # on disk unselected provides no compiler, so is_xcode_app_installed(),
    # which now means "installed anywhere", must not gate this.
    if (base::isTRUE(is_xcode_toolchain_usable())) {
        if(verbose) {
            cli::cli_alert_info("{.pkg macrtools}: A usable Xcode toolchain is already active.")
            cli::cli_bullets(c(
                "Location: {.path {developer_dir()}}",
                "Skipping the commandline installation."
            ))
            cli::cli_text("") # Add spacing
        }
        return(base::invisible(TRUE))
    }

    temporary_xcli_file <- "/tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress"

    # Create a temporary in-progress file
    base::file.create(temporary_xcli_file)

    if (verbose) {
        cli::cli_alert_info("{.pkg macrtools}: Checking for available Xcode CLI updates.")
        cli::cli_text("") # Add spacing
    }

    product_information <- xcode_cli_available_label()

    if (base::length(product_information) == 0) {
        # Remove temporary in-progress file if left in place before aborting.
        remove_file_if_exists(temporary_xcli_file)
        cli::cli_abort(c(
            "{.pkg macrtools}: Could not find Xcode CLI in software updates.",
            "i" = "Try installing manually with 'xcode-select --install' in Terminal."
        ))
    }

    if (verbose) {
        cli::cli_alert_info("{.pkg macrtools}: Installing Xcode CLI.")
        cli::cli_bullets(c(
            "Version: {.val {product_information}}",
            "This process may take 10-15 minutes. Please be patient."
        ))
        cli::cli_text("") # Add spacing
    }

    cmd <- base::paste("softwareupdate", "-i", base::shQuote(product_information), "--verbose")

    xcli_status <- shell_execute(cmd,
                                 sudo = TRUE, password = password, verbose = verbose)

    # Remove temporary in-progress file if left in place
    remove_file_if_exists(temporary_xcli_file)

    xcli_clean <- base::identical(xcli_status, 0L)

    if(base::isFALSE(xcli_clean)) {
        cli::cli_abort(c(
            "{.pkg macrtools}: We were not able to install Xcode CLI.",
            "i" = "Please try to manually install using: https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-cli.html#xcode-cli-installation"
        ))
    }

    if (verbose) {
        cli::cli_alert_success("{.pkg macrtools}: Xcode CLI installed successfully!")
        cli::cli_text("") # Add spacing
    }

    return(base::invisible(xcli_clean))
}

# Query softwareupdate for the label of the latest available Command Line Tools.
# Returns a length-0 character vector when none is found.
xcode_cli_available_label <- function() {
    base::system("softwareupdate -l |
          grep '\\*.*Command Line' |
          tail -n 1 |
          awk -F\"*\" '{print $2}' |
          sed -e 's/^ *//' |
          sed 's/Label: //g' |
          tr -d '\n'", intern = TRUE)
}


#' @section Uninstalling Xcode CLI:
#'
#' The `xcode_cli_uninstall()` attempts to remove _only_ the Xcode CLI tools.
#'
#' Per the [Apple Technical Note TN2339](https://developer.apple.com/library/archive/technotes/tn2339/_index.html#//apple_ref/doc/uid/DTS40014588-CH1-HOW_CAN_I_UNINSTALL_THE_COMMAND_LINE_TOOLS_):
#'
#' - Xcode includes all of the command-line tools. If it is installed on your system, remove it to uninstall the command-line tools.
#' - If the `/Library/Developer/CommandLineTools` directory exists on your system, remove it to uninstall the command-line tools
#'
#' Thus, the `xcode_cli_uninstall()` opts to perform the second step **only**.
#' We use an _R_ sanitized _shell_ version of:
#'
#' ```sh
#' sudo rm -rf /Library/Developer/CommandLineTools
#' ```
#'
#' If the Xcode application is detect, we note that we did not uninstall the
#' Xcode application. Instead, we request the user uninstall the Xcode
#' app using the following steps:
#'
#' 1. Make sure that Xcode is closed. Quit Xcode if needed.
#' 2. Open Finder > Applications, select Xcode and move it to Trash.
#' 3. Empty the trash.
#' 4. In addition, open Terminal and run:
#'
#' ```sh
#' sudo /Developer/Library/uninstall-devtools --mode=all
#' ```
#'
#' @export
#' @rdname xcode-cli
xcode_cli_uninstall <- function(password = base::getOption("macrtools.password"), verbose = TRUE){
    assert_mac()

    # Returns FALSE so callers cannot report a removal that never happened.
    if(base::isFALSE(is_xcode_cli_installed())) {
        if(verbose) {
            cli::cli_alert_info("{.pkg macrtools}: Xcode CLI is not installed.")
            cli::cli_text("") # Add spacing
        }
        return(base::invisible(FALSE))
    }

    # Removing the Command Line Tools invalidates the selection if they are the
    # active developer directory, so warn before, not after.
    active_dir <- developer_dir()
    if (!base::is.na(active_dir) &&
        base::identical(active_dir, normalize_developer_dir(install_directory_xcode_cli()))) {
        cli::cli_alert_warning(base::paste(
            "{.pkg macrtools}: The Command Line Tools are the active developer",
            "directory. After removal, select another toolchain with",
            "{.code xcode_cli_switch()}."
        ))
        cli::cli_text("") # Add spacing
    }

    if (verbose) {
        cli::cli_alert_info("{.pkg macrtools}: Uninstalling Xcode CLI.")
        cli::cli_text("") # Add spacing
    }

    # Remove the shell execution script
    xcli_uninstall_status <- shell_execute("rm -rf /Library/Developer/CommandLineTools",
                                           sudo = TRUE,
                                           password = password)

    xcli_uninstall_clean <- base::identical(xcli_uninstall_status, 0L)

    if(base::isFALSE(xcli_uninstall_clean)) {
        cli::cli_abort(c(
            "{.pkg macrtools}: We were not able to uninstall Xcode CLI.",
            "i" = "Please try to manually uninstall using: https://mac.thecoatlessprofessor.com/macrtools/reference/xcode-cli.html#uninstalling-xcode-cli"
        ))
    }

    if (verbose) {
        cli::cli_alert_success("{.pkg macrtools}: Xcode CLI uninstalled successfully!")
        cli::cli_text("") # Add spacing
    }

    base::invisible(xcli_uninstall_clean)
}


#' @section Change Xcode CLI Location:
#' The path reported by `xcode-select` may not point at the toolchain you want
#' to build against. [xcode_cli_switch()] changes it, defaulting to the
#' Command Line Tools:
#'
#' ```sh
#' sudo xcode-select --switch /Library/Developer/CommandLineTools
#' ```
#'
#' Pass `path` to select something else, using a `path` from
#' [xcode_toolchains()]. The target is validated first, and the selection being
#' replaced is named, since the change is machine-wide and affects every build.
#'
#' Note that `DEVELOPER_DIR` overrides the stored selection, so this has no
#' visible effect while that variable is set.
#'
#' If this does not fix the issue, we recommend using the [xcode_cli_reset()]
#' function.
#' @export
#' @rdname xcode-cli
#' @param path Developer directory, or `Xcode.app` bundle, to make active.
#'   Defaults to the Command Line Tools location. Use [xcode_toolchains()] to
#'   see the available options.
xcode_cli_switch <- function(path = install_directory_xcode_cli(),
                             password = base::getOption("macrtools.password"),
                             verbose = TRUE) {
    assert_mac()

    target <- normalize_developer_dir(path)

    # The value is deliberately NOT echoed. `path` used to be `password`, so a
    # legacy positional call lands here carrying a sudo password.
    if (!base::dir.exists(target) ||
        !base::dir.exists(base::file.path(target, "usr", "bin"))) {
        cli::cli_abort(c(
            "{.pkg macrtools}: The {.arg path} argument is not a developer directory.",
            "i" = "Run {.code xcode_toolchains()} to see the available toolchains."
        ))
    }

    # DEVELOPER_DIR overrides the stored selection for xcrun and the /usr/bin
    # shims, but `xcode-select --switch` writes the STORED selection. Comparing
    # against the override would skip a switch that has not happened, so the
    # stored value decides whether this is a no-op.
    stored <- base::tryCatch(xcode_select_path(), error = function(e) NULL)
    current <- if (!base::is.null(stored) && base::identical(stored$status, 0L) &&
                   base::length(stored$output) == 1L && base::nzchar(stored$output)) {
        normalize_developer_dir(stored$output)
    } else {
        NA_character_
    }

    if (base::nzchar(base::Sys.getenv("DEVELOPER_DIR", unset = "")) && verbose) {
        cli::cli_alert_warning(base::paste(
            "{.pkg macrtools}: {.envvar DEVELOPER_DIR} is set and overrides the",
            "stored selection, so this change will not take effect until it is",
            "unset."
        ))
        cli::cli_text("") # Add spacing
    }

    # The target is already the stored selection.
    if (!base::is.na(current) && base::identical(current, target)) {
        if (verbose) {
            cli::cli_alert_info("{.pkg macrtools}: Xcode CLI path is correctly set.")
            cli::cli_text("") # Add spacing
        }
        return(base::invisible(TRUE))
    }

    # Switching away from a deliberately selected toolchain is machine-wide and
    # affects every build, so say what is being displaced rather than doing it
    # quietly.
    if (verbose) {
        cli::cli_alert_info("{.pkg macrtools}: Setting the Xcode CLI path.")
        cli::cli_bullets(c(
            "Current path: {.path {if (base::is.na(current)) '<none selected>' else current}}",
            "Target path: {.path {target}}"
        ))
        cli::cli_text("") # Add spacing
    }

    cmd <- base::paste("xcode-select", "--switch", base::shQuote(target))

    # Change the directory
    xcli_switch_status <- shell_execute(cmd,
                                        sudo = TRUE,
                                        password = password,
                                        verbose = verbose)

    xcli_switch_clean <- base::identical(xcli_switch_status, 0L)

    if(base::isFALSE(xcli_switch_clean)) {
        cli::cli_abort("{.pkg macrtools}: Failed to switch Xcode CLI path.")
    }

    if (verbose) {
        cli::cli_alert_success("{.pkg macrtools}: Xcode CLI path updated successfully!")
        cli::cli_text("") # Add spacing
    }

    return(base::invisible(xcli_switch_clean))
}

#' @section Reset Xcode CLI:
#' The [xcode_cli_reset()] function uses `xcode-select` to restore
#' the default Xcode CLI settings.
#'
#' We use an _R_ sanitized _shell_ version of:
#'
#' ```sh
#' sudo xcode-select --reset
#' ```
#'
#' @export
#' @rdname xcode-cli
xcode_cli_reset <- function(password = base::getOption("macrtools.password"), verbose = TRUE) {
    if (verbose) {
        cli::cli_alert_info("{.pkg macrtools}: Resetting Xcode CLI to default settings.")
        cli::cli_text("") # Add spacing
    }

    cmd <- base::paste("xcode-select", "--reset")

    # Change the directory
    xcli_reset_status <- shell_execute(cmd,
                                       sudo = TRUE,
                                       password = password,
                                       verbose = verbose)

    xcli_reset_clean <- base::identical(xcli_reset_status, 0L)

    if(base::isFALSE(xcli_reset_clean)) {
        cli::cli_abort("{.pkg macrtools}: Failed to reset Xcode CLI settings.")
    }

    if(verbose) {
        cli::cli_alert_success("{.pkg macrtools}: Successfully reset Xcode CLI settings!")
        cli::cli_text("") # Add spacing
    }

    return(base::invisible(xcli_reset_clean))
}
