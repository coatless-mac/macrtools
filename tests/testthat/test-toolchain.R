test_that("macos_rtools_install performs system checks first", {
    # Mock system checks
    local_mocked_bindings(assert_mac = function() stop("Not macOS"))

    # CLI mocks to avoid output
    mockery::stub(macos_rtools_install, "cli::cli_h3", function(...) NULL)
    mockery::stub(macos_rtools_install, "cli::cli_text", function(...) NULL)
    mockery::stub(macos_rtools_install, "cli::cli_ul", function(...) NULL)

    expect_error(macos_rtools_install(), "Not macOS")

    # Reset mocks for assert_mac to pass but fail on next check
    local_mocked_bindings(
        assert_mac = function() NULL,
        assert_macos_supported = function() stop("Unsupported macOS")
    )

    expect_error(macos_rtools_install(), "Unsupported macOS")
})

test_that("macos_rtools_install orchestrates the component steps", {
    local_mocked_bindings(
        assert_mac = function() NULL,
        assert_macos_supported = function() NULL,
        assert_r_version_supported = function() NULL,
        shell_mac_version = function() "14.0",
        system_arch = function() "aarch64",
        rtools_install_announce = function(...) NULL,
        # The three component steps are the seams; orchestration just wires them up.
        rtools_install_xcode_cli = function(...) TRUE,
        rtools_install_gfortran = function(...) TRUE,
        rtools_install_recipes = function(...) TRUE,
        rtools_install_summary = function(x, g, b) x && g && base::isTRUE(b)
    )
    mockery::stub(macos_rtools_install, "base::Sys.info", function() c(release = "23.0"))

    # Progress-bar scaffolding lives in macos_rtools_install itself
    mockery::stub(macos_rtools_install, "cli::cli_progress_bar", function(...) 1)
    mockery::stub(macos_rtools_install, "cli::cli_progress_update", function(...) NULL)
    mockery::stub(macos_rtools_install, "cli::cli_progress_done", function(...) NULL)
    mockery::stub(macos_rtools_install, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(macos_rtools_install, "cli::cli_text", function(...) NULL)

    result <- macos_rtools_install(password = "password", verbose = TRUE)
    expect_true(result)
})

test_that("macos_rtools_install aborts when a component step fails", {
    local_mocked_bindings(
        assert_mac = function() NULL,
        assert_macos_supported = function() NULL,
        assert_r_version_supported = function() NULL,
        shell_mac_version = function() "14.0",
        system_arch = function() "aarch64",
        rtools_install_announce = function(...) NULL,
        rtools_install_xcode_cli = function(...) TRUE,
        rtools_install_gfortran = function(...) TRUE,
        rtools_install_recipes = function(...) FALSE
    )
    mockery::stub(macos_rtools_install, "base::Sys.info", function() c(release = "23.0"))

    mockery::stub(macos_rtools_install, "cli::cli_progress_bar", function(...) 1)
    mockery::stub(macos_rtools_install, "cli::cli_progress_update", function(...) NULL)
    mockery::stub(macos_rtools_install, "cli::cli_progress_done", function(...) NULL)
    mockery::stub(macos_rtools_install, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(macos_rtools_install, "cli::cli_text", function(...) NULL)

    # rtools_install_summary runs for real and aborts on the failed component
    expect_error(macos_rtools_install(password = "password", verbose = TRUE),
                 "Installation failed")
})

test_that("rtools_install_summary returns TRUE on success and aborts on failure", {
    mockery::stub(rtools_install_summary, "cli::cli_h3", function(...) NULL)
    mockery::stub(rtools_install_summary, "cli::cli_ul", function(...) NULL)
    mockery::stub(rtools_install_summary, "cli::cli_text", function(...) NULL)
    mockery::stub(rtools_install_summary, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(rtools_install_summary, "cli::cli_alert_success", function(...) NULL)
    local_mocked_bindings(timestamp_now = function(...) "now")

    expect_true(rtools_install_summary(TRUE, TRUE, TRUE))
    expect_error(rtools_install_summary(TRUE, FALSE, TRUE), "Installation failed")
})

test_that("rtools_install_xcode_cli installs when missing and aborts on failure", {
    mockery::stub(rtools_install_xcode_cli, "cli::cli_h3", function(...) NULL)
    mockery::stub(rtools_install_xcode_cli, "cli::cli_text", function(...) NULL)
    mockery::stub(rtools_install_xcode_cli, "cli::cli_ul", function(...) NULL)
    mockery::stub(rtools_install_xcode_cli, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(rtools_install_xcode_cli, "cli::cli_bullets", function(...) NULL)
    mockery::stub(rtools_install_xcode_cli, "cli::cli_progress_update", function(...) NULL)
    # Unusable before the install, usable after, which is what a successful
    # install actually looks like.
    installed <- FALSE
    local_mocked_bindings(
        is_xcode_toolchain_usable = function() installed,
        is_xcode_cli_installed = function() FALSE,
        xcode_cli_install = function(...) {
            installed <<- TRUE
            TRUE
        }
    )
    expect_true(rtools_install_xcode_cli("pw", FALSE, FALSE, NULL))

    installed <- FALSE
    local_mocked_bindings(xcode_cli_install = function(...) FALSE)
    expect_error(rtools_install_xcode_cli("pw", FALSE, FALSE, NULL), "Failed to install Xcode")
})

test_that("rtools_install_gfortran installs when missing and aborts on failure", {
    mockery::stub(rtools_install_gfortran, "cli::cli_h3", function(...) NULL)
    mockery::stub(rtools_install_gfortran, "cli::cli_text", function(...) NULL)
    mockery::stub(rtools_install_gfortran, "cli::cli_ul", function(...) NULL)
    mockery::stub(rtools_install_gfortran, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(rtools_install_gfortran, "cli::cli_bullets", function(...) NULL)
    mockery::stub(rtools_install_gfortran, "cli::cli_progress_update", function(...) NULL)
    local_mocked_bindings(
        is_gfortran_installed = function() FALSE,
        gfortran_install = function(...) TRUE
    )
    expect_true(rtools_install_gfortran("pw", FALSE, FALSE, NULL, "aarch64", "4.6.0"))

    local_mocked_bindings(gfortran_install = function(...) FALSE)
    expect_error(rtools_install_gfortran("pw", FALSE, FALSE, NULL, "aarch64", "4.6.0"),
                 "Failed to install GNU Fortran")
})

test_that("rtools_install_recipes returns the recipes install result", {
    mockery::stub(rtools_install_recipes, "cli::cli_h3", function(...) NULL)
    mockery::stub(rtools_install_recipes, "cli::cli_text", function(...) NULL)
    mockery::stub(rtools_install_recipes, "cli::cli_ul", function(...) NULL)
    local_mocked_bindings(recipes_binary_install = function(...) TRUE)

    expect_true(rtools_install_recipes("pw", FALSE, NULL, "aarch64"))
})

test_that("macos_rtools_uninstall handles component uninstallations", {
    # Mock CLI functions
    mockery::stub(macos_rtools_uninstall, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(macos_rtools_uninstall, "cli::cli_bullets", function(...) NULL)
    mockery::stub(macos_rtools_uninstall, "cli::cli_text", function(...) NULL)
    mockery::stub(macos_rtools_uninstall, "cli::cli_progress_bar", function(...) 1)
    mockery::stub(macos_rtools_uninstall, "cli::cli_progress_update", function(...) NULL)
    mockery::stub(macos_rtools_uninstall, "cli::cli_progress_done", function(...) NULL)
    mockery::stub(macos_rtools_uninstall, "cli::cli_alert_success", function(...) NULL)

    # Mock component detection and uninstallation
    local_mocked_bindings(
        force_password = function(...) "password",
        is_xcode_cli_installed = function() TRUE,
        xcode_cli_uninstall = function(...) TRUE,
        is_gfortran_installed = function() TRUE,
        gfortran_uninstall = function(...) TRUE
    )

    result <- macos_rtools_uninstall(verbose = TRUE)
    expect_true(result)

    # Test when components are not installed
    local_mocked_bindings(
        is_xcode_cli_installed = function() FALSE,
        is_gfortran_installed = function() FALSE
    )

    result <- macos_rtools_uninstall(verbose = TRUE)
    expect_true(result)
})

test_that("macos_rtools_uninstall handles component failures", {
    # Mock CLI functions
    mockery::stub(macos_rtools_uninstall, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(macos_rtools_uninstall, "cli::cli_bullets", function(...) NULL)
    mockery::stub(macos_rtools_uninstall, "cli::cli_text", function(...) NULL)
    mockery::stub(macos_rtools_uninstall, "cli::cli_progress_bar", function(...) 1)
    mockery::stub(macos_rtools_uninstall, "cli::cli_progress_update", function(...) NULL)
    mockery::stub(macos_rtools_uninstall, "cli::cli_abort", function(...) stop("Uninstallation failed"))

    # Mock component detection and uninstallation failure
    local_mocked_bindings(
        force_password = function(...) "password",
        is_xcode_cli_installed = function() TRUE,
        xcode_cli_uninstall = function(...) FALSE
    )

    expect_error(macos_rtools_uninstall(verbose = TRUE), "Uninstallation failed")

    # Test when gfortran fails
    local_mocked_bindings(
        is_xcode_cli_installed = function() FALSE,
        is_gfortran_installed = function() TRUE,
        gfortran_uninstall = function(...) FALSE
    )

    expect_error(macos_rtools_uninstall(verbose = TRUE), "Uninstallation failed")
})

test_that("rtools_install_xcode_cli skips installing when a toolchain is already usable", {
    # Regression: a versioned Xcode.app (what every GitHub runner selects) used
    # to be misread as "no toolchain" and triggered a 10-15 minute install of
    # Command Line Tools that were already present.
    mockery::stub(rtools_install_xcode_cli, "cli::cli_h3", function(...) NULL)
    mockery::stub(rtools_install_xcode_cli, "cli::cli_text", function(...) NULL)
    mockery::stub(rtools_install_xcode_cli, "cli::cli_ul", function(...) NULL)
    mockery::stub(rtools_install_xcode_cli, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(rtools_install_xcode_cli, "cli::cli_bullets", function(...) NULL)
    mockery::stub(rtools_install_xcode_cli, "cli::cli_progress_update", function(...) NULL)
    local_mocked_bindings(
        is_xcode_toolchain_usable = function() TRUE,
        developer_dir = function() "/Applications/Xcode_26.6.app/Contents/Developer",
        xcode_app_bundles = function() "/Applications/Xcode_26.6.app",
        xcode_app_version = function(bundle) "26.6",
        xcode_cli_install = function(...) stop("must not install when a toolchain is usable")
    )

    expect_true(rtools_install_xcode_cli("pw", FALSE, FALSE, NULL))
})

# Helper: capture the cli message templates emitted by the install step.
capture_xcode_cli_messages <- function(...) {
    seen <- base::character(0)
    record <- function(...) {
        seen <<- c(seen, base::unlist(base::list(...)))
        NULL
    }
    for (fn in c("cli::cli_h3", "cli::cli_text", "cli::cli_ul",
                 "cli::cli_progress_update")) {
        mockery::stub(rtools_install_xcode_cli, fn, function(...) NULL)
    }
    for (fn in c("cli::cli_alert_info", "cli::cli_alert_warning",
                 "cli::cli_alert_success", "cli::cli_bullets")) {
        mockery::stub(rtools_install_xcode_cli, fn, record)
    }
    result <- rtools_install_xcode_cli("pw", FALSE, TRUE, NULL)
    base::list(result = result, messages = base::paste(seen, collapse = " | "))
}

test_that("a CLT-only machine is not reported as having the full Xcode IDE", {
    # Regression: the install gate moved to is_xcode_toolchain_usable() but the
    # reporting branch still announced "Full Xcode.app IDE is installed" for
    # any usable toolchain, including a machine with no Xcode at all.
    local_mocked_bindings(
        is_xcode_toolchain_usable = function() TRUE,
        is_xcode_cli_installed = function() TRUE,
        developer_dir = function() "/Library/Developer/CommandLineTools",
        developer_dir_is_app = function(...) FALSE,
        xcode_cli_version = function() "27.0.0.0.1788430756",
        xcode_cli_install = function(...) stop("must not install")
    )

    out <- capture_xcode_cli_messages()

    expect_true(out$result)
    expect_no_match(out$messages, "Full Xcode.app IDE", fixed = TRUE)
    expect_match(out$messages, "Command Line Tools", fixed = TRUE)
})

test_that("an active full Xcode is reported as such, at its real location", {
    local_mocked_bindings(
        is_xcode_toolchain_usable = function() TRUE,
        is_xcode_cli_installed = function() FALSE,
        developer_dir = function() "/Applications/Xcode_26.6.app/Contents/Developer",
        developer_dir_is_app = function(...) TRUE,
        xcode_app_version = function(bundle) "26.6",
        xcode_cli_install = function(...) stop("must not install")
    )

    out <- capture_xcode_cli_messages()

    expect_true(out$result)
    expect_match(out$messages, "Xcode", fixed = TRUE)
})

test_that("CLT on disk with an unusable toolchain is reported as a failure", {
    # Regression: this state returned TRUE and printed "Pre-installed, no
    # action needed", so the install summary claimed success on a machine that
    # could not compile.
    local_mocked_bindings(
        is_xcode_toolchain_usable = function() FALSE,
        is_xcode_cli_installed = function() TRUE,
        developer_dir = function() NA_character_,
        xcode_cli_install = function(...) stop("reinstalling cannot fix a bad selection")
    )

    out <- capture_xcode_cli_messages()

    expect_false(out$result)
    expect_no_match(out$messages, "no action needed", fixed = TRUE)
})

test_that("an install that leaves the toolchain unreachable is not reported as success", {
    # Installing the Command Line Tools does not guarantee they became
    # reachable: a stale DEVELOPER_DIR or selection still shadows them, so the
    # component must verify rather than trust the installer's return value.
    local_mocked_bindings(
        is_xcode_toolchain_usable = function() FALSE,
        is_xcode_cli_installed = function() FALSE,
        developer_dir = function() "/nonexistent/stale",
        xcode_cli_install = function(...) TRUE
    )

    out <- capture_xcode_cli_messages()

    expect_false(out$result)
})

test_that("an install that yields a reachable toolchain is reported as success", {
    # First call gates the install, the second is the post-install check.
    calls <- 0L
    local_mocked_bindings(
        is_xcode_toolchain_usable = function() {
            calls <<- calls + 1L
            calls > 1L
        },
        is_xcode_cli_installed = function() FALSE,
        xcode_cli_install = function(...) TRUE
    )

    out <- capture_xcode_cli_messages()

    expect_true(out$result)
    expect_equal(calls, 2L)
})
