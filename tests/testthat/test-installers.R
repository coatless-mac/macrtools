test_that("binary_download handles successful downloads", {
    # Mock dependencies
    mockery::stub(binary_download, "base::tempdir", function() "/tmp")
    mockery::stub(binary_download, "base::file.path", function(...) "/tmp/test.tar.gz")
    mockery::stub(binary_download, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(binary_download, "cli::cli_bullets", function(...) NULL)
    mockery::stub(binary_download, "cli::cli_text", function(...) NULL)
    mockery::stub(binary_download, "cli::cli_progress_bar", function(...) 1)
    mockery::stub(binary_download, "cli::cli_progress_done", function(...) NULL)
    mockery::stub(binary_download, "cli::cli_alert_success", function(...) NULL)
    mockery::stub(binary_download, "base::Sys.time", function() Sys.time())
    mockery::stub(binary_download, "base::tryCatch",
                  function(expr, ...) {
                      # Simulate successful download
                      return(TRUE)
                  })
    mockery::stub(binary_download, "base::file.info", function(...) list(size = 1024 * 1024))
    mockery::stub(binary_download, "base::round", function(...) 1)
    mockery::stub(binary_download, "base::as.numeric", function(...) 10)

    result <- binary_download("https://example.com/test.tar.gz", verbose = TRUE)
    expect_equal(result, "/tmp/test.tar.gz")
})

test_that("binary_download handles download errors", {
    # Mock dependencies with error
    mockery::stub(binary_download, "base::tempdir", function() "/tmp")
    mockery::stub(binary_download, "base::file.path", function(...) "/tmp/test.tar.gz")
    mockery::stub(binary_download, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(binary_download, "cli::cli_bullets", function(...) NULL)
    mockery::stub(binary_download, "cli::cli_text", function(...) NULL)
    mockery::stub(binary_download, "cli::cli_progress_bar", function(...) 1)
    mockery::stub(binary_download, "cli::cli_progress_done", function(...) NULL)
    mockery::stub(binary_download, "cli::cli_abort", function(...) stop("Download failed"))
    mockery::stub(binary_download, "base::tryCatch",
                  function(expr, error, ...) {
                      # Simulate download error
                      error(simpleError("Connection failed"))
                  })

    expect_error(binary_download("https://example.com/test.tar.gz", verbose = TRUE),
                 "Download failed")
})

test_that("tar_package_install handles successful installation", {
    # Mock dependencies
    mockery::stub(tar_package_install, "base::shQuote", function(x) paste0("'", x, "'"))
    mockery::stub(tar_package_install, "base::normalizePath", function(path) path)
    mockery::stub(tar_package_install, "base::basename", function(path) "test.tar.gz")
    mockery::stub(tar_package_install, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(tar_package_install, "cli::cli_bullets", function(...) NULL)
    mockery::stub(tar_package_install, "cli::cli_text", function(...) NULL)
    mockery::stub(tar_package_install, "shell_execute", function(...) 0)
    mockery::stub(tar_package_install, "base::unlink", function(...) NULL)
    mockery::stub(tar_package_install, "cli::cli_alert_success", function(...) NULL)

    result <- tar_package_install("/tmp/test.tar.gz", "/opt", 2, verbose = TRUE)
    expect_true(result)
})

test_that("tar_package_install handles installation errors", {
    # Mock dependencies with error
    mockery::stub(tar_package_install, "base::shQuote", function(x) paste0("'", x, "'"))
    mockery::stub(tar_package_install, "base::normalizePath", function(path) path)
    mockery::stub(tar_package_install, "base::basename", function(path) "test.tar.gz")
    mockery::stub(tar_package_install, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(tar_package_install, "cli::cli_bullets", function(...) NULL)
    mockery::stub(tar_package_install, "cli::cli_text", function(...) NULL)
    mockery::stub(tar_package_install, "shell_execute", function(...) -1)
    mockery::stub(tar_package_install, "cli::cli_abort", function(...) stop("Installation failed"))

    expect_error(tar_package_install("/tmp/test.tar.gz", "/opt", 2, verbose = TRUE),
                 "Installation failed")
})

test_that("create_install_location succeeds when directory exists", {
    # Mock dependencies
    mockery::stub(create_install_location, "install_location", function(...) "/opt/test")
    mockery::stub(create_install_location, "base::dir.exists", function(...) TRUE)

    result <- create_install_location()
    expect_true(result)
})

test_that("pkg_install handles successful installation", {
    # Mock dependencies
    mockery::stub(pkg_install, "base::basename", function(path) "test.pkg")
    mockery::stub(pkg_install, "tools::file_path_sans_ext", function(path) "test")
    mockery::stub(pkg_install, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(pkg_install, "cli::cli_bullets", function(...) NULL)
    mockery::stub(pkg_install, "cli::cli_text", function(...) NULL)
    mockery::stub(pkg_install, "base::paste", function(...) "sudo -kS installer -pkg test.pkg -target /")
    mockery::stub(pkg_install, "shell_execute", function(...) 0)
    mockery::stub(pkg_install, "cli::cli_alert_success", function(...) NULL)
    mockery::stub(pkg_install, "cli::cli_abort", function(...) NULL)

    result <- pkg_install("/tmp/test.pkg", verbose = TRUE)
    expect_true(result)
})

test_that("recipe_binary_install_strip_level follows the toolchain tiers", {
    # Modern tier (R >= 4.3): strip 3 for every architecture
    mockery::stub(recipe_binary_install_strip_level, "is_r_version_supported", function(...) TRUE)
    mockery::stub(recipe_binary_install_strip_level, "is_r_version_at_least", function(...) TRUE)
    expect_equal(recipe_binary_install_strip_level("arm64"), 3)
    expect_equal(recipe_binary_install_strip_level("aarch64"), 3)
    expect_equal(recipe_binary_install_strip_level("x86_64"), 3)

    # Legacy tier (4.0 <= R < 4.3): delegates to install_strip_level()
    mockery::stub(recipe_binary_install_strip_level, "is_r_version_at_least", function(...) FALSE)
    mockery::stub(recipe_binary_install_strip_level, "install_strip_level", function(...) 99)
    expect_equal(recipe_binary_install_strip_level("arm64"), 99)

    # Unsupported R version: abort
    mockery::stub(recipe_binary_install_strip_level, "is_r_version_supported", function(...) FALSE)
    expect_error(recipe_binary_install_strip_level("arm64"), regexp = "Unsupported R version")
})

test_that("recipe_binary_install_location follows the toolchain tiers", {
    # Modern tier (R >= 4.3): /opt/R/<arch>
    mockery::stub(recipe_binary_install_location, "is_r_version_supported", function(...) TRUE)
    mockery::stub(recipe_binary_install_location, "is_r_version_at_least", function(...) TRUE)
    expect_equal(recipe_binary_install_location("arm64"), "/opt/R/arm64")
    expect_equal(recipe_binary_install_location("aarch64"), "/opt/R/arm64")
    expect_equal(recipe_binary_install_location("x86_64"), "/opt/R/x86_64")

    # Legacy tier (4.0 <= R < 4.3): delegates to install_location()
    mockery::stub(recipe_binary_install_location, "is_r_version_at_least", function(...) FALSE)
    mockery::stub(recipe_binary_install_location, "install_location", function(...) "/legacy/loc")
    expect_equal(recipe_binary_install_location("x86_64"), "/legacy/loc")

    # Unsupported R version: abort
    mockery::stub(recipe_binary_install_location, "is_r_version_supported", function(...) FALSE)
    expect_error(recipe_binary_install_location("arm64"), regexp = "Unsupported R version")
})

test_that("gfortran_install_location follows the toolchain tiers", {
    # Modern tier (R >= 4.3): /opt
    mockery::stub(gfortran_install_location, "is_r_version_supported", function(...) TRUE)
    mockery::stub(gfortran_install_location, "is_r_version_at_least", function(...) TRUE)
    expect_equal(gfortran_install_location("arm64"), "/opt")

    # Legacy tier (4.0 <= R < 4.3): delegates to install_location()
    mockery::stub(gfortran_install_location, "is_r_version_at_least", function(...) FALSE)
    mockery::stub(gfortran_install_location, "install_location", function(...) "/legacy/loc")
    expect_equal(gfortran_install_location("x86_64"), "/legacy/loc")

    # Unsupported R version: abort
    mockery::stub(gfortran_install_location, "is_r_version_supported", function(...) FALSE)
    expect_error(gfortran_install_location("arm64"), regexp = "Unsupported R version")
})

test_that("tar_package_install aborts on a positive (non-zero) exit status", {
    # A real tar failure exits with a positive status; it must be treated as a
    # failure, not silently ignored (regression test for status < 0 vs != 0).
    mockery::stub(tar_package_install, "base::shQuote", function(x) paste0("'", x, "'"))
    mockery::stub(tar_package_install, "base::normalizePath", function(path) path)
    mockery::stub(tar_package_install, "base::basename", function(path) "test.tar.gz")
    mockery::stub(tar_package_install, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(tar_package_install, "cli::cli_bullets", function(...) NULL)
    mockery::stub(tar_package_install, "cli::cli_text", function(...) NULL)
    mockery::stub(tar_package_install, "shell_execute", function(...) 1)
    mockery::stub(tar_package_install, "cli::cli_abort", function(...) stop("Installation failed"))

    expect_error(tar_package_install("/tmp/test.tar.gz", "/opt", 2, verbose = TRUE),
                 "Installation failed")
})
