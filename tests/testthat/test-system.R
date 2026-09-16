test_that("system_os returns the correct OS name", {
    mockery::stub(system_os, "base::Sys.info", function() c(sysname = "Darwin"))
    expect_equal(system_os(), "darwin")

    mockery::stub(system_os, "base::Sys.info", function() c(sysname = "Linux"))
    expect_equal(system_os(), "linux")
})

test_that("system_arch returns the correct architecture", {
    # We can't mock R.version, so test the function against the real value.
    expect_equal(system_arch(), base::R.version$arch)
})

test_that("is_aarch64 and is_x86_64 reflect the system architecture", {
    local_mocked_bindings(system_arch = function() "aarch64")
    expect_true(is_aarch64())
    expect_false(is_x86_64())

    local_mocked_bindings(system_arch = function() "x86_64")
    expect_false(is_aarch64())
    expect_true(is_x86_64())
})

test_that("is_macos correctly identifies macOS", {
    local_mocked_bindings(system_os = function() "darwin")
    expect_true(is_macos())

    local_mocked_bindings(system_os = function() "linux")
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
    # Mock the version helpers so we exercise the real is_r_version() against a
    # controlled R version, rather than testing a reimplemented copy.
    local_mocked_bindings(
        r_version_major_minor = function() "4.2",
        r_version_full = function() "4.2.1"
    )

    # Default compares major.minor
    expect_true(is_r_version("4.2"))
    expect_false(is_r_version("4.1"))

    # compare_major_minor = FALSE compares the full major.minor.patch
    expect_true(is_r_version("4.2.1", compare_major_minor = FALSE))
    expect_false(is_r_version("4.2.0", compare_major_minor = FALSE))
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

test_that("macOS support window is the single source of truth", {
    expect_equal(minimum_supported_macos_version(), "10.13.0")
    # The ceiling is EXCLUSIVE: it names the first version we reject, not the
    # newest we support. Writing the newest supported release here instead is
    # the off-by-one that shipped for Tahoe (#28) and again for macOS 27.
    expect_equal(first_unsupported_macos_version(), "28.0")
})

test_that("is_macos_r_supported accepts every release below the ceiling", {
    for (v in c("10.13.0", "10.15.7", "11.7.10", "14.7.2", "15.6.1",
                "26.0", "26.4.1", "27.0", "27.0.1", "27.9.9")) {
        local_mocked_bindings(shell_mac_version = function() v)
        expect_true(is_macos_r_supported(), info = v)
    }
})

test_that("is_macos_r_supported rejects releases outside the window", {
    for (v in c("10.12.0", "10.12.6", "28.0", "28.0.1", "29.0")) {
        local_mocked_bindings(shell_mac_version = function() v)
        expect_false(is_macos_r_supported(), info = v)
    }
})

test_that("is_macos_golden_gate identifies macOS 27", {
    for (v in c("27.0", "27.0.1", "27.3.2")) {
        local_mocked_bindings(shell_mac_version = function() v)
        expect_true(is_macos_golden_gate(), info = v)
    }
    for (v in c("26.4.1", "28.0")) {
        local_mocked_bindings(shell_mac_version = function() v)
        expect_false(is_macos_golden_gate(), info = v)
    }
})

macos_predicates <- function() {
    base::list(
        high_sierra = is_macos_high_sierra,
        mojave      = is_macos_mojave,
        catalina    = is_macos_catalina,
        big_sur     = is_macos_big_sur,
        monterey    = is_macos_monterey,
        ventura     = is_macos_ventura,
        sonoma      = is_macos_sonoma,
        sequoia     = is_macos_sequoia,
        tahoe       = is_macos_tahoe,
        golden_gate = is_macos_golden_gate
    )
}

test_that("exactly one named macOS predicate matches any given release", {
    # Guards against a new release both widening its own range and being
    # swallowed by its predecessor's (e.g. bumping is_macos_tahoe() to 28.0).
    predicates <- macos_predicates()

    samples <- c(
        high_sierra = "10.13.6", mojave   = "10.14.6", catalina = "10.15.7",
        big_sur     = "11.7.10", monterey = "12.7.6",  ventura  = "13.7.1",
        sonoma      = "14.7.2",  sequoia  = "15.6.1",  tahoe    = "26.4.1",
        golden_gate = "27.0.1"
    )

    for (nm in base::names(samples)) {
        local_mocked_bindings(shell_mac_version = function() samples[[nm]])
        matched <- base::names(base::Filter(function(f) f(), predicates))
        expect_equal(matched, nm, info = samples[[nm]])
    }
})

test_that("named macOS predicates handle the two-component form of a .0 release", {
    # sw_vers -productVersion reports only two components for a .0 release:
    # macOS 27.0 reports "27.0", never "27.0.0". compareVersion() ranks a
    # shorter string BELOW an otherwise-equal longer one, so an unpadded
    # two-component version compared against three-component bounds lands in
    # the PREVIOUS release's range.
    predicates <- macos_predicates()

    samples <- c(
        high_sierra = "10.13", mojave   = "10.14", catalina = "10.15",
        big_sur     = "11.0",  monterey = "12.0",  ventura  = "13.0",
        sonoma      = "14.0",  sequoia  = "15.0",  tahoe    = "26.0",
        golden_gate = "27.0"
    )

    for (nm in base::names(samples)) {
        local_mocked_bindings(shell_mac_version = function() samples[[nm]])
        matched <- base::names(base::Filter(function(f) f(), predicates))
        expect_equal(matched, nm, info = samples[[nm]])
    }
})

test_that("is_macos_r_supported accepts the two-component form of its floor", {
    # High Sierra GM reports "10.13". Rejecting it produces an error message
    # that names 10.13 as supported in the same breath.
    local_mocked_bindings(shell_mac_version = function() "10.13")
    expect_true(is_macos_r_supported())
})

test_that("version_between handles double-digit majors", {
    # 10.x-only coverage would hide a lexical-vs-numeric comparison mistake.
    expect_true(version_between("27.0", "10.13.0", "28.0"))
    expect_true(version_between("9.0", "9.0", "28.0"))
    expect_false(version_between("28.0", "10.13.0", "28.0"))
    expect_false(version_between("28.0.1", "10.13.0", "28.0"))
})

test_that("pad_version pads to three components without truncating", {
    expect_equal(pad_version("27"), "27.0.0")
    expect_equal(pad_version("27.0"), "27.0.0")
    expect_equal(pad_version("27.0.1"), "27.0.1")
    expect_equal(pad_version("10.13"), "10.13.0")
    # Longer versions are left alone rather than cut down.
    expect_equal(pad_version("27.0.1.2"), "27.0.1.2")
    # Padding must make the .0 form compare equal to its padded self.
    expect_equal(utils::compareVersion(pad_version("27.0"), pad_version("27.0.0")), 0L)
})
test_that("the advertised macOS window is derived from the enforced bound", {
    # The range text must follow the bound rather than repeat it, which is how
    # the advertised window silently went stale in past releases.
    expect_match(macos_support_range(), "\\b27\\.x\\b")

    local_mocked_bindings(first_unsupported_macos_version = function() "29.0")
    expect_match(macos_support_range(), "\\b28\\.x\\b")
})
