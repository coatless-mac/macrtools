test_that("xcode_select_path returns correct output", {
    # Mock successful xcode-select call
    mock_output <- list(
        stdout = charToRaw("/Library/Developer/CommandLineTools"),
        stderr = charToRaw(""),
        status = 0L
    )

    mockery::stub(xcode_select_path, "xcode_select", function(...) {
        base::structure(
            base::list(
                output = "/Library/Developer/CommandLineTools",
                error = "",
                status = 0L
            ),
            class = c("xcodeselect", "cli")
        )
    })

    result <- xcode_select_path()
    expect_equal(result$status, 0L)
    expect_equal(result$output, "/Library/Developer/CommandLineTools")
})

test_that("is_xcode_cli_installed correctly identifies installed CLI tools", {
    # First, mock dependencies for success scenario
    mockery::stub(is_xcode_cli_installed, "assert_mac", function() TRUE)

    # Mock successful path detection
    mockery::stub(is_xcode_cli_installed, "xcode_select_path", function() {
        base::structure(
            base::list(
                output = "/Library/Developer/CommandLineTools",
                error = "",
                status = 0L
            ),
            class = c("xcodeselect", "cli")
        )
    })

    # Mock installation directory exists
    mockery::stub(is_xcode_cli_installed, "install_directory_xcode_cli", function() "/Library/Developer/CommandLineTools")
    mockery::stub(is_xcode_cli_installed, "base::dir.exists", function(path) TRUE)

    expect_true(is_xcode_cli_installed())

    # Now mock failure scenarios
    mockery::stub(is_xcode_cli_installed, "assert_mac", function() TRUE)
    mockery::stub(is_xcode_cli_installed, "xcode_select_path", function() {
        base::structure(
            base::list(
                output = "/Library/Developer/CommandLineTools",
                error = "",
                status = 1L  # Non-zero status
            ),
            class = c("xcodeselect", "cli")
        )
    })

    expect_false(is_xcode_cli_installed())
})

test_that("is_xcode_app_installed correctly identifies installed Xcode app", {
    # Mock dependencies for success scenario
    mockery::stub(is_xcode_app_installed, "assert_mac", function() TRUE)

    # Mock successful path detection
    mockery::stub(is_xcode_app_installed, "xcode_select_path", function() {
        base::structure(
            base::list(
                output = "/Applications/Xcode.app/Contents/Developer",
                error = "",
                status = 0L
            ),
            class = c("xcodeselect", "cli")
        )
    })

    # Mock installation directory exists
    mockery::stub(is_xcode_app_installed, "install_directory_xcode_app", function() "/Applications/Xcode.app/Contents/Developer")
    mockery::stub(is_xcode_app_installed, "base::dir.exists", function(path) TRUE)

    expect_true(is_xcode_app_installed())

    # Now mock failure scenarios
    mockery::stub(is_xcode_app_installed, "xcode_select_path", function() {
        base::structure(
            base::list(
                output = "/Library/Developer/CommandLineTools",  # Wrong path
                error = "",
                status = 0L
            ),
            class = c("xcodeselect", "cli")
        )
    })

    expect_false(is_xcode_app_installed())
})

test_that("xcode_cli_path returns correct path", {
    # Mock successful xcode-select call
    mockery::stub(xcode_cli_path, "xcode_select_path", function() {
        base::structure(
            base::list(
                output = "/Library/Developer/CommandLineTools",
                error = "",
                status = 0L
            ),
            class = c("xcodeselect", "cli")
        )
    })

    expect_equal(xcode_cli_path(), "/Library/Developer/CommandLineTools")

    # Mock failed xcode-select call
    mockery::stub(xcode_cli_path, "xcode_select_path", function() {
        base::structure(
            base::list(
                output = "",
                error = "Error: command not found",
                status = 1L
            ),
            class = c("xcodeselect", "cli")
        )
    })

    expect_equal(xcode_cli_path(), "")
})

test_that("xcode_cli_install skips when already installed", {
    # Mock successful check
    mockery::stub(xcode_cli_install, "assert_mac", function() TRUE)
    mockery::stub(xcode_cli_install, "is_xcode_cli_installed", function() TRUE)
    mockery::stub(xcode_cli_install, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(xcode_cli_install, "cli::cli_text", function(...) NULL)

    result <- xcode_cli_install(verbose = TRUE)
    expect_true(result)
})

test_that("xcode_cli_install skips when Xcode app is installed", {
    # Mock CLI not installed but Xcode app is
    mockery::stub(xcode_cli_install, "assert_mac", function() TRUE)
    mockery::stub(xcode_cli_install, "is_xcode_cli_installed", function() FALSE)
    mockery::stub(xcode_cli_install, "is_xcode_app_installed", function() TRUE)
    mockery::stub(xcode_cli_install, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(xcode_cli_install, "cli::cli_bullets", function(...) NULL)
    mockery::stub(xcode_cli_install, "cli::cli_text", function(...) NULL)

    result <- xcode_cli_install(verbose = TRUE)
    expect_true(result)
})
