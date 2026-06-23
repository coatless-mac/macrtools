test_that("assert succeeds when condition is TRUE", {
    expect_no_error(assert(TRUE, "This should not error"))
})

test_that("assert throws error when condition is FALSE", {
    expect_error(assert(FALSE, "Error message"), regexp = "Error message")
})

test_that("assert_mac succeeds on macOS", {
    local_mocked_bindings(is_macos = function() TRUE)
    expect_no_error(assert_mac())
})

test_that("assert_mac throws error on non-macOS", {
    local_mocked_bindings(is_macos = function() FALSE, system_os = function() "linux")
    expect_error(assert_mac(), regexp = "This function requires macOS")
})

test_that("assert_mac surfaces the guidance advice in the error message", {
    local_mocked_bindings(is_macos = function() FALSE, system_os = function() "linux")
    # The advice line is folded into the message as an info bullet and must
    # render (it was previously passed as a swallowed `advice=` argument).
    expect_error(assert_mac(), regexp = "Intel or Apple Silicon processors")
})

test_that("assert_macos_supported succeeds on supported macOS version", {
    local_mocked_bindings(
        assert_mac = function(...) NULL,
        is_macos_r_supported = function() TRUE
    )
    expect_no_error(assert_macos_supported())
})

test_that("assert_macos_supported throws error on unsupported macOS version", {
    local_mocked_bindings(
        assert_mac = function(...) NULL,
        is_macos_r_supported = function() FALSE,
        shell_mac_version = function() "10.12"
    )
    expect_error(assert_macos_supported(), regexp = "not supported")
})

test_that("assert_aarch64 succeeds on Apple Silicon", {
    local_mocked_bindings(is_aarch64 = function() TRUE)
    expect_no_error(assert_aarch64())
})

test_that("assert_aarch64 throws error on non-Apple Silicon", {
    local_mocked_bindings(is_aarch64 = function() FALSE, system_arch = function() "x86_64")
    expect_error(assert_aarch64(), regexp = "requires an Apple Silicon")
})

test_that("assert_x86_64 succeeds on Intel", {
    local_mocked_bindings(is_x86_64 = function() TRUE)
    expect_no_error(assert_x86_64())
})

test_that("assert_x86_64 throws error on non-Intel", {
    local_mocked_bindings(is_x86_64 = function() FALSE, system_arch = function() "aarch64")
    expect_error(assert_x86_64(), regexp = "requires an Intel-based Mac")
})

test_that("assert_r_version_supported succeeds on supported R version", {
    local_mocked_bindings(is_r_version_supported = function(...) TRUE)
    expect_no_error(assert_r_version_supported())
})

test_that("assert_r_version_supported throws error on unsupported R version", {
    local_mocked_bindings(
        is_r_version_supported = function(...) FALSE,
        r_version_full = function() "3.6.0"
    )
    expect_error(assert_r_version_supported(), regexp = "not supported")
})
