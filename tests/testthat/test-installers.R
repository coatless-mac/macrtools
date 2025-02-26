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
