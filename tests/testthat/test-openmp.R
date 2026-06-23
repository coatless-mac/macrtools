test_that("is_openmp_installed checks for the library and header", {
    local_mocked_bindings(assert_mac = function(...) NULL)

    mockery::stub(is_openmp_installed, "base::file.exists", function(p) TRUE)
    expect_true(is_openmp_installed())

    mockery::stub(is_openmp_installed, "base::file.exists", function(p) FALSE)
    expect_false(is_openmp_installed())
})

test_that("get_openmp_url_for_xcode maps Apple clang build numbers to runtimes", {
    pick <- function(build) {
        local_mocked_bindings(
            get_apple_clang_version = function() list(build_number = build)
        )
        get_openmp_url_for_xcode()
    }

    # Xcode 16.3+ / Xcode 26 (clang 1700.x) -> newest runtime
    expect_match(pick(1700), "openmp-19.1.5-darwin20-Release.tar.gz")
    # Xcode 16.0-16.2 (clang 1600.x)
    expect_match(pick(1600), "openmp-17.0.6-darwin20-Release.tar.gz")
    # Xcode 14.0-14.2 (clang 1400.x)
    expect_match(pick(1400), "openmp-14.0.6-darwin20-Release.tar.gz")
    # A build newer than anything mapped still resolves to the newest runtime
    expect_match(pick(9999), "openmp-19.1.5-darwin20-Release.tar.gz")
    # Undetectable clang (build 0) falls back to the newest runtime
    expect_match(pick(0), "openmp-19.1.5-darwin20-Release.tar.gz")
})

test_that("openmp_uninstall honors configure_makevars without erroring", {
    # Regression: openmp_uninstall() previously referenced an undefined
    # `remove_makevars_config`, erroring after removing the libraries.
    local_mocked_bindings(
        assert_mac = function(...) NULL,
        is_openmp_installed = function() TRUE,
        remove_openmp_makevars_config = function(...) TRUE
    )
    mockery::stub(openmp_uninstall, "base::file.exists", function(...) FALSE)
    mockery::stub(openmp_uninstall, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(openmp_uninstall, "cli::cli_bullets", function(...) NULL)
    mockery::stub(openmp_uninstall, "cli::cli_text", function(...) NULL)
    mockery::stub(openmp_uninstall, "cli::cli_alert_success", function(...) NULL)

    expect_true(openmp_uninstall(password = "pw", verbose = TRUE, configure_makevars = TRUE))
})
