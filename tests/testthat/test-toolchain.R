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

test_that("macos_rtools_install handles component installations", {
    # Mock all system checks
    mockery::stub(macos_rtools_install, "assert_mac", function() NULL)
    mockery::stub(macos_rtools_install, "assert_macos_supported", function() NULL)
    mockery::stub(macos_rtools_install, "assert_r_version_supported", function() NULL)

    # Mock system info gathering
    mockery::stub(macos_rtools_install, "shell_mac_version", function() "14.0")
    mockery::stub(macos_rtools_install, "system_arch", function() "aarch64")
    mockery::stub(macos_rtools_install, "base::Sys.info", function() c(release = "23.0"))
    mockery::stub(macos_rtools_install, "base::R.version", list(major = "4", minor = "3"))
    mockery::stub(macos_rtools_install, "base::paste", function(...) "4.3")
    mockery::stub(macos_rtools_install, "base::tryCatch", function(...) 10)
    mockery::stub(macos_rtools_install, "base::round", function(...) 10)
    mockery::stub(macos_rtools_install, "base::as.numeric", function(...) 10)
    mockery::stub(macos_rtools_install, "base::format", function(...) "2023-01-01 12:00:00")
    mockery::stub(macos_rtools_install, "base::Sys.time", function() Sys.time())

    # Mock all CLI functions
    mockery::stub(macos_rtools_install, "cli::cli_h3", function(...) NULL)
    mockery::stub(macos_rtools_install, "cli::cli_text", function(...) NULL)
    mockery::stub(macos_rtools_install, "cli::cli_ul", function(...) NULL)
    mockery::stub(macos_rtools_install, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(macos_rtools_install, "cli::cli_bullets", function(...) NULL)
    mockery::stub(macos_rtools_install, "cli::cli_progress_bar", function(...) 1)
    mockery::stub(macos_rtools_install, "cli::cli_progress_update", function(...) NULL)
    mockery::stub(macos_rtools_install, "cli::cli_progress_done", function(...) NULL)
    mockery::stub(macos_rtools_install, "cli::cli_alert_success", function(...) NULL)
    mockery::stub(macos_rtools_install, "cli::cli_code", function(...) NULL)

    # Mock password entry
    mockery::stub(macos_rtools_install, "askpass::askpass", function(...) "password")

    # Mock Xcode installation
    mockery::stub(macos_rtools_install, "is_xcode_app_installed", function() FALSE)
    mockery::stub(macos_rtools_install, "is_xcode_cli_installed", function() FALSE)
    mockery::stub(macos_rtools_install, "xcode_cli_install", function(...) TRUE)

    # Mock gfortran installation
    mockery::stub(macos_rtools_install, "is_gfortran_installed", function() FALSE)
    mockery::stub(macos_rtools_install, "gfortran_install", function(...) TRUE)

    # Mock recipe installation
    mockery::stub(macos_rtools_install, "recipe_binary_install_location", function(...) "/opt/R/arm64")
    mockery::stub(macos_rtools_install, "recipes_binary_install", function(...) TRUE)

    # Mock version info for summary
    mockery::stub(macos_rtools_install, "base::tryCatch", function(...) "Mock version info")
    mockery::stub(macos_rtools_install, "base::substr", function(...) "Mock version")
    mockery::stub(macos_rtools_install, "base::paste0", function(...) "Mock version...")
    mockery::stub(macos_rtools_install, "base::nchar", function(...) 30)

    result <- macos_rtools_install(verbose = TRUE)
    expect_true(result)
})

test_that("macos_rtools_install handles component failures", {
    # Mock all system checks
    mockery::stub(macos_rtools_install, "assert_mac", function() NULL)
    mockery::stub(macos_rtools_install, "assert_macos_supported", function() NULL)
    mockery::stub(macos_rtools_install, "assert_r_version_supported", function() NULL)

    # Mock system info gathering
    mockery::stub(macos_rtools_install, "shell_mac_version", function() "14.0")
    mockery::stub(macos_rtools_install, "system_arch", function() "aarch64")
    mockery::stub(macos_rtools_install, "base::Sys.info", function() c(release = "23.0"))
    mockery::stub(macos_rtools_install, "base::R.version", list(major = "4", minor = "3"))
    mockery::stub(macos_rtools_install, "base::paste", function(...) "4.3")
    mockery::stub(macos_rtools_install, "base::tryCatch", function(...) 10)
    mockery::stub(macos_rtools_install, "base::round", function(...) 10)
    mockery::stub(macos_rtools_install, "base::as.numeric", function(...) 10)

    # Mock all CLI functions
    mockery::stub(macos_rtools_install, "cli::cli_h3", function(...) NULL)
    mockery::stub(macos_rtools_install, "cli::cli_text", function(...) NULL)
    mockery::stub(macos_rtools_install, "cli::cli_ul", function(...) NULL)
    mockery::stub(macos_rtools_install, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(macos_rtools_install, "cli::cli_bullets", function(...) NULL)
    mockery::stub(macos_rtools_install, "cli::cli_progress_bar", function(...) 1)
    mockery::stub(macos_rtools_install, "cli::cli_progress_update", function(...) NULL)
    mockery::stub(macos_rtools_install, "cli::cli_progress_done", function(...) NULL)
    mockery::stub(macos_rtools_install, "cli::cli_abort", function(...) stop("Installation failed"))

    # Mock component failures
    mockery::stub(macos_rtools_install, "is_xcode_app_installed", function() FALSE)
    mockery::stub(macos_rtools_install, "is_xcode_cli_installed", function() FALSE)
    mockery::stub(macos_rtools_install, "xcode_cli_install", function(...) FALSE)
    mockery::stub(macos_rtools_install, "is_gfortran_installed", function() FALSE)
    mockery::stub(macos_rtools_install, "gfortran_install", function(...) FALSE)
    mockery::stub(macos_rtools_install, "recipes_binary_install", function(...) FALSE)

    expect_error(macos_rtools_install(verbose = TRUE), "Installation failed")
})

test_that("macos_rtools_uninstall handles component uninstallations", {
    # Mock CLI functions
    mockery::stub(macos_rtools_uninstall, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(macos_rtools_uninstall, "cli::cli_bullets", function(...) NULL)
    mockery::stub(macos_rtools_uninstall, "cli::cli_text", function(...) NULL)
    mockery::stub(macos_rtools_uninstall, "askpass::askpass", function(...) "password")
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
