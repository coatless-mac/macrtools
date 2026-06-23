test_that("macos_rtools_install performs system checks first", {
    # Mock system checks
    mockery::stub(macos_rtools_install, "assert_mac", function() stop("Not macOS"))

    # CLI mocks to avoid output
    mockery::stub(macos_rtools_install, "cli::cli_h3", function(...) NULL)
    mockery::stub(macos_rtools_install, "cli::cli_text", function(...) NULL)
    mockery::stub(macos_rtools_install, "cli::cli_ul", function(...) NULL)

    expect_error(macos_rtools_install(), "Not macOS")

    # Reset mocks for assert_mac to pass but fail on next check
    mockery::stub(macos_rtools_install, "assert_mac", function() NULL)
    mockery::stub(macos_rtools_install, "assert_macos_supported", function() stop("Unsupported macOS"))

    expect_error(macos_rtools_install(), "Unsupported macOS")
})

test_that("macos_rtools_install orchestrates the component steps", {
    mockery::stub(macos_rtools_install, "assert_mac", function() NULL)
    mockery::stub(macos_rtools_install, "assert_macos_supported", function() NULL)
    mockery::stub(macos_rtools_install, "assert_r_version_supported", function() NULL)
    mockery::stub(macos_rtools_install, "shell_mac_version", function() "14.0")
    mockery::stub(macos_rtools_install, "system_arch", function() "aarch64")
    mockery::stub(macos_rtools_install, "base::Sys.info", function() c(release = "23.0"))
    mockery::stub(macos_rtools_install, "rtools_install_announce", function(...) NULL)

    # The three component steps are the seams; orchestration just wires them up.
    mockery::stub(macos_rtools_install, "rtools_install_xcode_cli", function(...) TRUE)
    mockery::stub(macos_rtools_install, "rtools_install_gfortran", function(...) TRUE)
    mockery::stub(macos_rtools_install, "rtools_install_recipes", function(...) TRUE)
    mockery::stub(macos_rtools_install, "rtools_install_summary",
                  function(x, g, b) x && g && base::isTRUE(b))

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
    mockery::stub(macos_rtools_install, "assert_mac", function() NULL)
    mockery::stub(macos_rtools_install, "assert_macos_supported", function() NULL)
    mockery::stub(macos_rtools_install, "assert_r_version_supported", function() NULL)
    mockery::stub(macos_rtools_install, "shell_mac_version", function() "14.0")
    mockery::stub(macos_rtools_install, "system_arch", function() "aarch64")
    mockery::stub(macos_rtools_install, "base::Sys.info", function() c(release = "23.0"))
    mockery::stub(macos_rtools_install, "rtools_install_announce", function(...) NULL)

    mockery::stub(macos_rtools_install, "rtools_install_xcode_cli", function(...) TRUE)
    mockery::stub(macos_rtools_install, "rtools_install_gfortran", function(...) TRUE)
    mockery::stub(macos_rtools_install, "rtools_install_recipes", function(...) FALSE)

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
    mockery::stub(rtools_install_summary, "timestamp_now", function(...) "now")

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
    mockery::stub(rtools_install_xcode_cli, "is_xcode_app_installed", function() FALSE)
    mockery::stub(rtools_install_xcode_cli, "is_xcode_cli_installed", function() FALSE)

    mockery::stub(rtools_install_xcode_cli, "xcode_cli_install", function(...) TRUE)
    expect_true(rtools_install_xcode_cli("pw", FALSE, FALSE, NULL))

    mockery::stub(rtools_install_xcode_cli, "xcode_cli_install", function(...) FALSE)
    expect_error(rtools_install_xcode_cli("pw", FALSE, FALSE, NULL), "Failed to install Xcode")
})

test_that("rtools_install_gfortran installs when missing and aborts on failure", {
    mockery::stub(rtools_install_gfortran, "cli::cli_h3", function(...) NULL)
    mockery::stub(rtools_install_gfortran, "cli::cli_text", function(...) NULL)
    mockery::stub(rtools_install_gfortran, "cli::cli_ul", function(...) NULL)
    mockery::stub(rtools_install_gfortran, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(rtools_install_gfortran, "cli::cli_bullets", function(...) NULL)
    mockery::stub(rtools_install_gfortran, "cli::cli_progress_update", function(...) NULL)
    mockery::stub(rtools_install_gfortran, "is_gfortran_installed", function() FALSE)

    mockery::stub(rtools_install_gfortran, "gfortran_install", function(...) TRUE)
    expect_true(rtools_install_gfortran("pw", FALSE, FALSE, NULL, "aarch64", "4.6.0"))

    mockery::stub(rtools_install_gfortran, "gfortran_install", function(...) FALSE)
    expect_error(rtools_install_gfortran("pw", FALSE, FALSE, NULL, "aarch64", "4.6.0"),
                 "Failed to install GNU Fortran")
})

test_that("rtools_install_recipes returns the recipes install result", {
    mockery::stub(rtools_install_recipes, "cli::cli_h3", function(...) NULL)
    mockery::stub(rtools_install_recipes, "cli::cli_text", function(...) NULL)
    mockery::stub(rtools_install_recipes, "cli::cli_ul", function(...) NULL)
    mockery::stub(rtools_install_recipes, "recipes_binary_install", function(...) TRUE)

    expect_true(rtools_install_recipes("pw", FALSE, NULL, "aarch64"))
})

test_that("macos_rtools_uninstall handles component uninstallations", {
    # Mock CLI functions
    mockery::stub(macos_rtools_uninstall, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(macos_rtools_uninstall, "cli::cli_bullets", function(...) NULL)
    mockery::stub(macos_rtools_uninstall, "cli::cli_text", function(...) NULL)
    mockery::stub(macos_rtools_uninstall, "force_password", function(...) "password")
    mockery::stub(macos_rtools_uninstall, "cli::cli_progress_bar", function(...) 1)
    mockery::stub(macos_rtools_uninstall, "cli::cli_progress_update", function(...) NULL)
    mockery::stub(macos_rtools_uninstall, "cli::cli_progress_done", function(...) NULL)
    mockery::stub(macos_rtools_uninstall, "cli::cli_alert_success", function(...) NULL)

    # Mock component detection and uninstallation
    mockery::stub(macos_rtools_uninstall, "is_xcode_cli_installed", function() TRUE)
    mockery::stub(macos_rtools_uninstall, "xcode_cli_uninstall", function(...) TRUE)
    mockery::stub(macos_rtools_uninstall, "is_gfortran_installed", function() TRUE)
    mockery::stub(macos_rtools_uninstall, "gfortran_uninstall", function(...) TRUE)

    result <- macos_rtools_uninstall(verbose = TRUE)
    expect_true(result)

    # Test when components are not installed
    mockery::stub(macos_rtools_uninstall, "is_xcode_cli_installed", function() FALSE)
    mockery::stub(macos_rtools_uninstall, "is_gfortran_installed", function() FALSE)

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
    mockery::stub(macos_rtools_uninstall, "force_password", function(...) "password")

    # Mock component detection and uninstallation failure
    mockery::stub(macos_rtools_uninstall, "is_xcode_cli_installed", function() TRUE)
    mockery::stub(macos_rtools_uninstall, "xcode_cli_uninstall", function(...) FALSE)

    expect_error(macos_rtools_uninstall(verbose = TRUE), "Uninstallation failed")

    # Test when gfortran fails
    mockery::stub(macos_rtools_uninstall, "is_xcode_cli_installed", function() FALSE)
    mockery::stub(macos_rtools_uninstall, "is_gfortran_installed", function() TRUE)
    mockery::stub(macos_rtools_uninstall, "gfortran_uninstall", function(...) FALSE)

    expect_error(macos_rtools_uninstall(verbose = TRUE), "Uninstallation failed")
})
