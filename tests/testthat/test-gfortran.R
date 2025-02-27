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
    mockery::stub(gfortran_install, "is_r_version", function(v) v == "4.3")
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

test_that("gfortran_install handles Intel Mac with R 4.2", {
    # Mock dependencies
    mockery::stub(gfortran_install, "assert_mac", function() NULL)
    mockery::stub(gfortran_install, "assert_macos_supported", function() NULL)
    mockery::stub(gfortran_install, "assert_r_version_supported", function() NULL)
    mockery::stub(gfortran_install, "is_gfortran_installed", function() FALSE)
    mockery::stub(gfortran_install, "is_r_version", function(v) v == "4.2")
    mockery::stub(gfortran_install, "is_x86_64", function() TRUE)
    mockery::stub(gfortran_install, "is_aarch64", function() FALSE)
    mockery::stub(gfortran_install, "gfortran_install_location", function() "/usr/local")
    mockery::stub(gfortran_install, "base::file.path", function(...) "/usr/local/gfortran/bin")
    mockery::stub(gfortran_install, "base::paste0", function(...) "$PATH:/usr/local/gfortran/bin")
    mockery::stub(gfortran_install, "force_password", function(pw) "mockpw")
    mockery::stub(gfortran_install, "create_install_location", function(...) TRUE)
    mockery::stub(gfortran_install, "install_gfortran_82_mojave", function(...) TRUE)
    mockery::stub(gfortran_install, "renviron_gfortran_path", function(...) NULL)
    mockery::stub(gfortran_install, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(gfortran_install, "cli::cli_bullets", function(...) NULL)
    mockery::stub(gfortran_install, "cli::cli_text", function(...) NULL)
    mockery::stub(gfortran_install, "cli::cli_alert_success", function(...) NULL)

    result <- gfortran_install(verbose = TRUE)
    expect_true(result)
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
