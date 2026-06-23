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
    local_mocked_bindings(
        is_xcode_app_installed = function() FALSE,
        is_xcode_cli_installed = function() FALSE,
        xcode_cli_install = function(...) TRUE
    )
    expect_true(rtools_install_xcode_cli("pw", FALSE, FALSE, NULL))

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
