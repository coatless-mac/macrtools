.onAttach <- function(libname, pkgname) {
    # Only run on interactive mode
    if (!base::interactive()) return()

    # Check if it's actually macOS
    if (!is_macos()) {
        os_info <- base::tolower(base::Sys.info()[['sysname']])
        base::packageStartupMessage(cli::format_inline(
            "{.pkg macrtools}: {.emph Warning: This package is designed for macOS only.}",
            "\n{.pkg macrtools}: {.emph Current OS: {.val {os_info}}}",
            "\n{.pkg macrtools}: {.emph See https://mac.thecoatlessprofessor.com/macrtools/ for more information.}"
        ))
        return()
    }

    # Check supported macOS version
    if (!is_macos_r_supported()) {
        mac_version <- shell_mac_version()
        supported_msg <- base::paste0(
            "Supported macOS versions: ",
            "High Sierra (10.13) through Tahoe (26.x)"
        )

        base::packageStartupMessage(cli::format_inline(
            "{.pkg macrtools}: {.emph Warning: macOS version {.val {mac_version}} may not be fully supported.}",
            "\n{.pkg macrtools}: {.emph {supported_msg}}",
            "\n{.pkg macrtools}: {.emph Some features may not work correctly on your system.}"
        ))
    } else {
        # Show welcome message on supported system
        if (base::getOption("macrtools.show_welcome", TRUE)) {
            mac_version <- shell_mac_version()
            base::packageStartupMessage(cli::format_inline(
                "{.pkg macrtools}: Ready to set up R development tools on macOS {.val {mac_version}}.",
                "\n{.pkg macrtools}: Run {.code macrtools::macos_rtools_install()} to begin installation.",
                "\n{.pkg macrtools}: For help, see: {.url https://mac.thecoatlessprofessor.com/macrtools/}"
            ))
        }
    }
}

.onLoad <- function(libname, pkgname) {
    # Import rlang functions
    rlang::check_installed("rlang")

    # Set cli options
    op <- base::options()
    op.macrtools <- base::list(
        macrtools.password = NULL,
        macrtools.show_welcome = TRUE,
        cli.width = 80,
        cli.progress_bar_style = "fillrect",
        cli.num_colors = if (cli::num_ansi_colors() >= 256) 256 else 1
    )

    toset <- !(base::names(op.macrtools) %in% base::names(op))
    if (base::any(toset)) base::options(op.macrtools[toset])

    base::invisible()
}
