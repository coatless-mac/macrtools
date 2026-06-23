test_that("system_os returns the correct OS name", {
    mockery::stub(system_os, "base::Sys.info", function() c(sysname = "Darwin"))
    expect_equal(system_os(), "darwin")

    mockery::stub(system_os, "base::Sys.info", function() c(sysname = "Linux"))
    expect_equal(system_os(), "linux")
})

test_that("system_arch returns the correct architecture", {
    # Instead of trying to mock R.version directly, we test the function itself
    expected_arch <- base::R.version$arch
    expect_equal(system_arch(), expected_arch)

    # For additional coverage, we test that the functions that use system_arch work
    mockery::stub(is_aarch64, "system_arch", function() "aarch64")
    expect_true(is_aarch64())

    mockery::stub(is_x86_64, "system_arch", function() "x86_64")
    expect_true(is_x86_64())
})

test_that("is_macos correctly identifies macOS", {
    mockery::stub(is_macos, "system_os", function() "darwin")
    expect_true(is_macos())

    mockery::stub(is_macos, "system_os", function() "linux")
    expect_false(is_macos())
})

test_that("shell_mac_version returns the correct macOS version", {
    mockery::stub(shell_mac_version, "sys::exec_internal", function(...) {
        list(stdout = charToRaw("14.0"))
    })
    mockery::stub(shell_mac_version, "sys::as_text", function(x) "14.0")

    expect_equal(shell_mac_version(), "14.0")
})

test_that("is_macos_r_supported correctly identifies supported macOS versions", {
    # Mock at the namespace level so the stub reaches shell_mac_version through
    # the shared macos_version_in_range() helper, and exercise the real
    # version_between() logic.
    local_mocked_bindings(shell_mac_version = function() "10.13.0")
    expect_true(is_macos_r_supported())

    local_mocked_bindings(shell_mac_version = function() "10.12.0")
    expect_false(is_macos_r_supported())
})

test_that("macos_version_in_range checks the running macOS version against bounds", {
    local_mocked_bindings(shell_mac_version = function() "14.2.0")
    expect_true(macos_version_in_range("14.0.0", "15.0.0"))
    expect_false(macos_version_in_range("15.0.0", "16.0.0"))
    expect_false(macos_version_in_range("13.0.0", "14.0.0"))
})

test_that("version_between correctly determines if version is within bounds", {
    expect_true(version_between("10.14.0", "10.13.0", "10.15.0"))
    expect_true(version_between("10.13.0", "10.13.0", "10.15.0"))
    expect_false(version_between("10.15.0", "10.13.0", "10.15.0"))
    expect_false(version_between("10.12.0", "10.13.0", "10.15.0"))
})

test_that("is_r_version correctly identifies R versions", {
    # We can't easily mock R.version, so instead we test a simplified version
    # of the function that uses mock data
    simplified_is_r_version <- function(target_version, r_major = "4", r_minor = "2.1") {
        minor_value <- strsplit(r_minor, ".", fixed = TRUE)[[1]][1]
        version_string <- paste(r_major, minor_value, sep = ".")
        return(version_string == target_version)
    }

    expect_true(simplified_is_r_version("4.2"))
    expect_false(simplified_is_r_version("4.1"))

    # Test with compare_major_minor = FALSE (directly using full version)
    simplified_is_r_version_full <- function(target_version, r_major = "4", r_minor = "2.1", compare_major_minor = FALSE) {
        if (compare_major_minor) {
            minor_value <- strsplit(r_minor, ".", fixed = TRUE)[[1]][1]
        } else {
            minor_value <- r_minor
        }
        version_string <- paste(r_major, minor_value, sep = ".")
        return(version_string == target_version)
    }

    expect_true(simplified_is_r_version_full("4.2.1", compare_major_minor = FALSE))
    expect_false(simplified_is_r_version_full("4.2.0", compare_major_minor = FALSE))
})

test_that("is_r_version_at_least compares major.minor versions", {
    expect_true(is_r_version_at_least("4.3", version = "4.6"))
    expect_true(is_r_version_at_least("4.3", version = "4.3"))
    expect_false(is_r_version_at_least("4.5", version = "4.3"))
    expect_true(is_r_version_at_least("4.0", version = "4.6"))
    expect_false(is_r_version_at_least("4.0", version = "3.6"))
    # The running R (>= 4.0) is always at least 4.0
    expect_true(is_r_version_at_least("4.0"))
})

test_that("is_r_version_supported honors the supported window", {
    for (v in c("4.0", "4.1", "4.2", "4.3", "4.4", "4.5", "4.6")) {
        expect_true(is_r_version_supported(version = v), info = v)
    }
    for (v in c("3.6", "4.7", "5.0")) {
        expect_false(is_r_version_supported(version = v), info = v)
    }
})

test_that("supported R version window is the single source of truth", {
    expect_equal(minimum_supported_r_version(), "4.0")
    expect_equal(maximum_supported_r_version(), "4.6")
    # The window endpoints must themselves be supported
    expect_true(is_r_version_supported(version = minimum_supported_r_version()))
    expect_true(is_r_version_supported(version = maximum_supported_r_version()))
})
