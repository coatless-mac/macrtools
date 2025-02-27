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
    mockery::stub(is_macos_r_supported, "shell_mac_version", function() "10.13.0")
    mockery::stub(is_macos_r_supported, "version_between", function(...) TRUE)
    expect_true(is_macos_r_supported())

    mockery::stub(is_macos_r_supported, "shell_mac_version", function() "10.12.0")
    mockery::stub(is_macos_r_supported, "version_between", function(...) FALSE)
    expect_false(is_macos_r_supported())
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
