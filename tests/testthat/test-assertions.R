test_that("assert succeeds when condition is TRUE", {
    expect_no_error(assert(TRUE, "This should not error"))
})

test_that("assert throws error when condition is FALSE", {
    expect_error(assert(FALSE, "Error message"), regexp = "Error message")
})

test_that("assert_mac succeeds on macOS", {
    mockery::stub(assert_mac, "is_macos", function() TRUE)
    expect_no_error(assert_mac())
})

test_that("assert_mac throws error on non-macOS", {
    mockery::stub(assert_mac, "is_macos", function() FALSE)
    mockery::stub(assert_mac, "base::Sys.info", function() c(sysname = "Linux"))
    expect_error(assert_mac(), regexp = "This function requires macOS")
})

test_that("assert_macos_supported succeeds on supported macOS version", {
    # Create a stub that correctly handles the call parameter
    mockery::stub(assert_macos_supported, "assert_mac", function(...) NULL)
    mockery::stub(assert_macos_supported, "is_macos_r_supported", function() TRUE)
    expect_no_error(assert_macos_supported())
})

test_that("assert_macos_supported throws error on unsupported macOS version", {
    # Create a stub that correctly handles the call parameter
    mockery::stub(assert_macos_supported, "assert_mac", function(...) NULL)
    mockery::stub(assert_macos_supported, "is_macos_r_supported", function() FALSE)
    mockery::stub(assert_macos_supported, "shell_mac_version", function() "10.12")
    # Mock cli::cli_abort to track error message but not actually throw
    mockery::stub(assert_macos_supported, "cli::cli_abort",
                  function(message, ...) stop(paste(message[1], collapse=" ")))

    expect_error(assert_macos_supported(), regexp = "not supported")
})

test_that("assert_aarch64 succeeds on Apple Silicon", {
    mockery::stub(assert_aarch64, "is_aarch64", function() TRUE)
    expect_no_error(assert_aarch64())
})

test_that("assert_aarch64 throws error on non-Apple Silicon", {
    mockery::stub(assert_aarch64, "is_aarch64", function() FALSE)
    mockery::stub(assert_aarch64, "system_arch", function() "x86_64")
    expect_error(assert_aarch64(), regexp = "requires an Apple Silicon")
})

test_that("assert_x86_64 succeeds on Intel", {
    mockery::stub(assert_x86_64, "is_x86_64", function() TRUE)
    expect_no_error(assert_x86_64())
})

test_that("assert_x86_64 throws error on non-Intel", {
    mockery::stub(assert_x86_64, "is_x86_64", function() FALSE)
    mockery::stub(assert_x86_64, "system_arch", function() "aarch64")
    expect_error(assert_x86_64(), regexp = "requires an Intel-based Mac")
})

test_that("assert_r_version_supported succeeds on supported R version", {
    # Instead of mocking R.version, mock is_r_version to return the expected values
    mockery::stub(assert_r_version_supported, "is_r_version",
                  function(version) version %in% c("4.0", "4.1", "4.2", "4.3", "4.4"))
    expect_no_error(assert_r_version_supported())
})

test_that("assert_r_version_supported throws error on unsupported R version", {
    # Mock is_r_version to return FALSE for any input
    mockery::stub(assert_r_version_supported, "is_r_version", function(...) FALSE)
    # Instead of trying to mock R.version, mock paste to return a known version
    mockery::stub(assert_r_version_supported, "base::paste", function(...) "3.6.0")
    # Mock cli::cli_abort to track error message but not actually throw
    mockery::stub(assert_r_version_supported, "cli::cli_abort",
                  function(message, ...) stop(paste(message[1], collapse=" ")))

    expect_error(assert_r_version_supported(), regexp = "not supported")
})
