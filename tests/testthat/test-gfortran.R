test_that("is_gfortran_installed correctly identifies installed gfortran", {
    # Mock dependencies for success scenario
    mockery::stub(is_gfortran_installed, "assert_mac", function() NULL)
    mockery::stub(is_gfortran_installed, "gfortran_install_location", function() "/opt")
    mockery::stub(is_gfortran_installed, "base::file.path", function(...) "/opt/gfortran")
    mockery::stub(is_gfortran_installed, "base::dir.exists", function(path) TRUE)

    expect_true(is_gfortran_installed())

    # Mock failure scenario
    mockery::stub(is_gfortran_installed, "base::dir.exists", function(path) FALSE)
    expect_false(is_gfortran_installed())
})

test_that("gfortran_version returns correct output", {
    mock_output <- "GNU Fortran (GCC) 9.3.0"
    mockery::stub(gfortran_version, "gfortran", function(...) {
        base::structure(
            base::list(
                output = mock_output,
                status = 0L
            ),
            class = c("gfortran", "cli")
        )
    })

    result <- gfortran_version()
    expect_equal(result$output, mock_output)
    expect_equal(result$status, 0L)
})

test_that("gfortran_install skips when already installed", {
    # Mock dependencies
    mockery::stub(gfortran_install, "assert_mac", function() NULL)
    mockery::stub(gfortran_install, "assert_macos_supported", function() NULL)
    mockery::stub(gfortran_install, "assert_r_version_supported", function() NULL)
    mockery::stub(gfortran_install, "is_gfortran_installed", function() TRUE)
    mockery::stub(gfortran_install, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(gfortran_install, "cli::cli_bullets", function(...) NULL)
    mockery::stub(gfortran_install, "cli::cli_text", function(...) NULL)
    mockery::stub(gfortran_install, "base::tryCatch", function(...) "Mock version")
    mockery::stub(gfortran_install, "base::file.path", function(...) "/opt/gfortran")

    result <- gfortran_install(verbose = TRUE)
    expect_true(result)
})

test_that("gfortran_install installs correct version for R 4.3+", {
    # Mock dependencies
    mockery::stub(gfortran_install, "assert_mac", function() NULL)
    mockery::stub(gfortran_install, "assert_macos_supported", function() NULL)
    mockery::stub(gfortran_install, "assert_r_version_supported", function() NULL)
    mockery::stub(gfortran_install, "is_gfortran_installed", function() FALSE)
    mockery::stub(gfortran_install, "is_r_version_at_least",
                  function(target, ...) utils::compareVersion("4.3", target) >= 0)
    mockery::stub(gfortran_install, "gfortran_install_location", function() "/opt")
    mockery::stub(gfortran_install, "base::file.path", function(...) "/opt/gfortran/bin")
    mockery::stub(gfortran_install, "base::paste0", function(...) "$PATH:/opt/gfortran/bin")
    mockery::stub(gfortran_install, "force_password", function(pw) "mockpw")
    mockery::stub(gfortran_install, "create_install_location", function(...) TRUE)
    mockery::stub(gfortran_install, "install_gfortran_12_2_universal", function(...) TRUE)
    mockery::stub(gfortran_install, "renviron_gfortran_path", function(...) NULL)
    mockery::stub(gfortran_install, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(gfortran_install, "cli::cli_bullets", function(...) NULL)
    mockery::stub(gfortran_install, "cli::cli_text", function(...) NULL)
    mockery::stub(gfortran_install, "cli::cli_alert_success", function(...) NULL)

    result <- gfortran_install(verbose = TRUE)
    expect_true(result)
})

test_that("gfortran_install uses the 14.2 universal installer for R 4.6", {
    # Mock dependencies
    mockery::stub(gfortran_install, "assert_mac", function() NULL)
    mockery::stub(gfortran_install, "assert_macos_supported", function() NULL)
    mockery::stub(gfortran_install, "assert_r_version_supported", function() NULL)
    mockery::stub(gfortran_install, "is_gfortran_installed", function() FALSE)
    mockery::stub(gfortran_install, "is_r_version_at_least",
                  function(target, ...) utils::compareVersion("4.6", target) >= 0)
    mockery::stub(gfortran_install, "gfortran_install_location", function() "/opt")
    mockery::stub(gfortran_install, "base::file.path", function(...) "/opt/gfortran/bin")
    mockery::stub(gfortran_install, "base::paste0", function(...) "$PATH:/opt/gfortran/bin")
    mockery::stub(gfortran_install, "force_password", function(pw) "mockpw")
    mockery::stub(gfortran_install, "create_install_location", function(...) TRUE)
    # The 14.2 universal installer must be chosen for R 4.6; fail loudly otherwise.
    mockery::stub(gfortran_install, "install_gfortran_14_2_universal", function(...) TRUE)
    mockery::stub(gfortran_install, "install_gfortran_12_2_universal",
                  function(...) stop("wrong installer: 12.2 used for R 4.6"))
    mockery::stub(gfortran_install, "renviron_gfortran_path", function(...) NULL)
    mockery::stub(gfortran_install, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(gfortran_install, "cli::cli_bullets", function(...) NULL)
    mockery::stub(gfortran_install, "cli::cli_text", function(...) NULL)
    mockery::stub(gfortran_install, "cli::cli_alert_success", function(...) NULL)

    result <- gfortran_install(verbose = TRUE)
    expect_true(result)
})

test_that("gfortran_install delegates to the legacy installer for R 4.2", {
    # Mock dependencies
    mockery::stub(gfortran_install, "assert_mac", function() NULL)
    mockery::stub(gfortran_install, "assert_macos_supported", function() NULL)
    mockery::stub(gfortran_install, "assert_r_version_supported", function() NULL)
    mockery::stub(gfortran_install, "is_gfortran_installed", function() FALSE)
    mockery::stub(gfortran_install, "is_r_version_at_least",
                  function(target, ...) utils::compareVersion("4.2", target) >= 0)
    mockery::stub(gfortran_install, "gfortran_install_location", function() "/usr/local")
    mockery::stub(gfortran_install, "base::file.path", function(...) "/usr/local/gfortran/bin")
    mockery::stub(gfortran_install, "base::paste0", function(...) "$PATH:/usr/local/gfortran/bin")
    mockery::stub(gfortran_install, "force_password", function(pw) "mockpw")
    mockery::stub(gfortran_install, "create_install_location", function(...) TRUE)
    mockery::stub(gfortran_install, "install_gfortran_legacy", function(...) TRUE)
    mockery::stub(gfortran_install, "renviron_gfortran_path", function(...) NULL)
    mockery::stub(gfortran_install, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(gfortran_install, "cli::cli_bullets", function(...) NULL)
    mockery::stub(gfortran_install, "cli::cli_text", function(...) NULL)
    mockery::stub(gfortran_install, "cli::cli_alert_success", function(...) NULL)

    result <- gfortran_install(verbose = TRUE)
    expect_true(result)
})

test_that("install_gfortran_legacy dispatches by architecture and R version", {
    # Intel -> gfortran 8.2 Mojave DMG installer
    mockery::stub(install_gfortran_legacy, "is_x86_64", function() TRUE)
    mockery::stub(install_gfortran_legacy, "install_gfortran_82_mojave", function(...) TRUE)
    expect_true(install_gfortran_legacy("pw", verbose = FALSE))

    # Apple Silicon + R 4.2 -> gfortran 12 arm tarball
    mockery::stub(install_gfortran_legacy, "is_x86_64", function() FALSE)
    mockery::stub(install_gfortran_legacy, "is_aarch64", function() TRUE)
    mockery::stub(install_gfortran_legacy, "is_r_version", function(v) v == "4.2")
    mockery::stub(install_gfortran_legacy, "install_gfortran_12_arm", function(...) TRUE)
    expect_true(install_gfortran_legacy("pw", verbose = FALSE))

    # Apple Silicon + R 4.0 -> abort (Apple Silicon began in R 4.1)
    mockery::stub(install_gfortran_legacy, "is_r_version", function(v) FALSE)
    expect_error(install_gfortran_legacy("pw", verbose = FALSE), "Apple Silicon")

    # Neither Intel nor Apple Silicon -> unsupported architecture abort
    mockery::stub(install_gfortran_legacy, "is_x86_64", function() FALSE)
    mockery::stub(install_gfortran_legacy, "is_aarch64", function() FALSE)
    mockery::stub(install_gfortran_legacy, "system_arch", function() "ppc")
    expect_error(install_gfortran_legacy("pw", verbose = FALSE), "Unsupported macOS architecture")
})

test_that("gfortran_uninstall succeeds when not installed", {
    # Mock dependencies
    mockery::stub(gfortran_uninstall, "assert_mac", function() NULL)
    mockery::stub(gfortran_uninstall, "is_gfortran_installed", function() FALSE)
    mockery::stub(gfortran_uninstall, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(gfortran_uninstall, "cli::cli_text", function(...) NULL)

    result <- gfortran_uninstall(verbose = TRUE)
    expect_true(result)
})

test_that("gfortran_uninstall removes installed gfortran", {
    # Mock dependencies with a proper file.path mock
    mockery::stub(gfortran_uninstall, "assert_mac", function() NULL)
    mockery::stub(gfortran_uninstall, "is_gfortran_installed", function() TRUE)
    mockery::stub(gfortran_uninstall, "gfortran_install_location", function() "/opt")

    # Create a more flexible file.path mock
    file_path_mock <- function(...) {
        args <- list(...)
        if (length(args) == 2 && args[[2]] == "gfortran") {
            return("/opt/gfortran")
        } else if (length(args) == 3 && args[[2]] == "bin" && args[[3]] == "gfortran") {
            return("/opt/bin/gfortran")
        }
        return(do.call(base::file.path, args))
    }
    mockery::stub(gfortran_uninstall, "base::file.path", file_path_mock)

    mockery::stub(gfortran_uninstall, "base::paste0", function(...) "rm -rf /opt/gfortran /opt/bin/gfortran")
    mockery::stub(gfortran_uninstall, "shell_execute", function(...) 0L)
    mockery::stub(gfortran_uninstall, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(gfortran_uninstall, "cli::cli_bullets", function(...) NULL)
    mockery::stub(gfortran_uninstall, "cli::cli_text", function(...) NULL)
    mockery::stub(gfortran_uninstall, "cli::cli_alert_success", function(...) NULL)

    result <- gfortran_uninstall(verbose = TRUE)
    expect_true(result)
})

test_that("gfortran() surfaces the error message when the binary is missing", {
    # Regression test: the error handler must carry the message in $stdout so the
    # caller (which reads out$stdout) does not silently drop it.
    mockery::stub(gfortran, "sys::exec_internal", function(...) stop("gfortran not found"))

    res <- gfortran("--version")
    expect_equal(res$status, -127L)
    expect_match(paste(res$output, collapse = " "), "gfortran not found")
})
