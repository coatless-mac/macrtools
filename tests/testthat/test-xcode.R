test_that("xcode_select_path returns correct output", {
    # Mock successful xcode-select call
    mock_output <- list(
        stdout = charToRaw("/Library/Developer/CommandLineTools"),
        stderr = charToRaw(""),
        status = 0L
    )

    local_mocked_bindings(
        xcode_select = function(...) {
            base::structure(
                base::list(
                    output = "/Library/Developer/CommandLineTools",
                    error = "",
                    status = 0L
                ),
                class = c("xcodeselect", "cli")
            )
        }
    )

    result <- xcode_select_path()
    expect_equal(result$status, 0L)
    expect_equal(result$output, "/Library/Developer/CommandLineTools")
})

test_that("is_xcode_cli_installed reports CLT package presence, not the selection", {
    # New semantics: the CLT package either is or is not installed. Which
    # developer directory happens to be selected is a different question,
    # answered by is_xcode_toolchain_usable().
    local_mocked_bindings(
        assert_mac = function() TRUE,
        install_directory_xcode_cli = function() "/Library/Developer/CommandLineTools"
    )

    # The stub is argument-aware so the test pins WHICH path is probed, not
    # merely that file.exists() was consulted.
    clang <- "/Library/Developer/CommandLineTools/usr/bin/clang"
    mockery::stub(is_xcode_cli_installed, "base::file.exists",
                  function(path) base::identical(path, clang))
    expect_true(is_xcode_cli_installed())

    mockery::stub(is_xcode_cli_installed, "base::file.exists",
                  function(path) base::identical(path, "/some/other/clang"))
    expect_false(is_xcode_cli_installed())
})

test_that("is_xcode_cli_installed ignores a non-standard active developer directory", {
    # The reported bug: a versioned Xcode.app is selected, CLT is installed,
    # and the old exact-equality predicate returned FALSE.
    local_mocked_bindings(
        assert_mac = function() TRUE,
        install_directory_xcode_cli = function() "/Library/Developer/CommandLineTools",
        xcode_select_path = function() {
            base::structure(
                base::list(
                    output = "/Applications/Xcode_16.4.app/Contents/Developer",
                    error = "",
                    status = 0L
                ),
                class = c("xcodeselect", "cli")
            )
        }
    )
    mockery::stub(
        is_xcode_cli_installed, "base::file.exists",
        function(path) base::identical(
            path, "/Library/Developer/CommandLineTools/usr/bin/clang"
        )
    )

    expect_true(is_xcode_cli_installed())
})

test_that("is_xcode_app_installed accepts any Xcode bundle location", {
    local_mocked_bindings(
        assert_mac = function() TRUE,
        xcode_app_bundles = function() "/Applications/Xcode_16.4.app"
    )
    expect_true(is_xcode_app_installed())

    local_mocked_bindings(
        assert_mac = function() TRUE,
        xcode_app_bundles = function() base::character(0)
    )
    expect_false(is_xcode_app_installed())
})

test_that("xcode_cli_path returns correct path", {
    # Mock successful xcode-select call
    local_mocked_bindings(
        xcode_select_path = function() {
            base::structure(
                base::list(
                    output = "/Library/Developer/CommandLineTools",
                    error = "",
                    status = 0L
                ),
                class = c("xcodeselect", "cli")
            )
        }
    )

    expect_equal(xcode_cli_path(), "/Library/Developer/CommandLineTools")

    # Mock failed xcode-select call
    local_mocked_bindings(
        xcode_select_path = function() {
            base::structure(
                base::list(
                    output = "",
                    error = "Error: command not found",
                    status = 1L
                ),
                class = c("xcodeselect", "cli")
            )
        }
    )

    expect_equal(xcode_cli_path(), "")
})

test_that("xcode_cli_install skips when already installed", {
    # Mock successful check
    local_mocked_bindings(
        assert_mac = function() TRUE,
        is_xcode_cli_installed = function() TRUE
    )
    mockery::stub(xcode_cli_install, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(xcode_cli_install, "cli::cli_text", function(...) NULL)

    result <- xcode_cli_install(verbose = TRUE)
    expect_true(result)
})

test_that("xcode_cli_install skips when Xcode app is installed", {
    # Mock CLI not installed but Xcode app is
    local_mocked_bindings(
        assert_mac = function() TRUE,
        is_xcode_cli_installed = function() FALSE,
        is_xcode_app_installed = function() TRUE
    )
    mockery::stub(xcode_cli_install, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(xcode_cli_install, "cli::cli_bullets", function(...) NULL)
    mockery::stub(xcode_cli_install, "cli::cli_text", function(...) NULL)

    result <- xcode_cli_install(verbose = TRUE)
    expect_true(result)
})

test_that("normalize_developer_dir expands a bundle path and strips trailing slashes", {
    # Deliberately under a root that cannot exist. normalizePath() resolves
    # symlinks for real paths, and on CI runners /Applications/Xcode.app is a
    # symlink to the versioned default, so using real paths here would assert
    # the filesystem rather than this function's own logic.
    root <- "/nonexistent-macrtools-test"

    expect_equal(
        normalize_developer_dir(base::file.path(root, "Xcode_16.4.app")),
        base::file.path(root, "Xcode_16.4.app/Contents/Developer")
    )
    expect_equal(
        normalize_developer_dir(base::file.path(root, "CommandLineTools/")),
        base::file.path(root, "CommandLineTools")
    )
    # An already-expanded developer directory is left alone.
    expect_equal(
        normalize_developer_dir(base::file.path(root, "Xcode.app/Contents/Developer")),
        base::file.path(root, "Xcode.app/Contents/Developer")
    )
})

test_that("normalize_developer_dir is vectorized", {
    # xcode_toolchains() normalizes a whole column at once, so a scalar-only
    # implementation errors with "the condition has length > 1".
    root <- "/nonexistent-macrtools-test"

    expect_equal(
        normalize_developer_dir(base::file.path(root, c(
            "Xcode_16.4.app",
            "CommandLineTools",
            "Xcode.app/Contents/Developer"
        ))),
        base::file.path(root, c(
            "Xcode_16.4.app/Contents/Developer",
            "CommandLineTools",
            "Xcode.app/Contents/Developer"
        ))
    )
    expect_equal(normalize_developer_dir(base::character(0)), base::character(0))
})

test_that("normalize_developer_dir resolves a symlinked bundle", {
    # This is what makes exact path equality unusable: a runner's
    # /Applications/Xcode.app is a symlink to the versioned bundle it defaults
    # to, so the two spellings must compare equal after normalization.
    scratch <- base::file.path(base::tempdir(), "macrtools-symlink-test")
    base::dir.create(base::file.path(scratch, "Real.app", "Contents", "Developer"),
                     recursive = TRUE, showWarnings = FALSE)
    on.exit(base::unlink(scratch, recursive = TRUE), add = TRUE)

    link <- base::file.path(scratch, "Link.app")
    skip_if_not(base::file.symlink(base::file.path(scratch, "Real.app"), link),
                "could not create a symlink")

    expect_equal(
        normalize_developer_dir(link),
        normalize_developer_dir(base::file.path(scratch, "Real.app"))
    )
})

test_that("developer_dir prefers DEVELOPER_DIR over the stored selection", {
    # DEVELOPER_DIR overrides xcode-select for xcrun and the /usr/bin shims,
    # so it must win here too.
    local_mocked_bindings(
        xcode_select_path = function() {
            base::structure(
                base::list(output = "/Library/Developer/CommandLineTools",
                           error = "", status = 0L),
                class = c("xcodeselect", "cli")
            )
        }
    )
    mockery::stub(developer_dir, "base::Sys.getenv",
                  function(...) "/Applications/Xcode_16.4.app/Contents/Developer")
    mockery::stub(developer_dir, "base::dir.exists", function(path) TRUE)

    expect_equal(developer_dir(), "/Applications/Xcode_16.4.app/Contents/Developer")
})

test_that("developer_dir falls back to the stored selection and is NA when unusable", {
    local_mocked_bindings(
        xcode_select_path = function() {
            base::structure(
                base::list(output = "/Applications/Xcode_26.6.app/Contents/Developer",
                           error = "", status = 0L),
                class = c("xcodeselect", "cli")
            )
        }
    )
    mockery::stub(developer_dir, "base::Sys.getenv", function(...) "")
    mockery::stub(developer_dir, "base::dir.exists", function(path) TRUE)
    expect_equal(developer_dir(), "/Applications/Xcode_26.6.app/Contents/Developer")

    # xcode-select echoes a path and exits 0 even when it does not exist, so a
    # stale selection must resolve to NA rather than a bogus path.
    mockery::stub(developer_dir, "base::dir.exists", function(path) FALSE)
    expect_true(base::is.na(developer_dir()))
})

test_that("developer_dir is NA when nothing is selected", {
    local_mocked_bindings(
        xcode_select_path = function() {
            base::structure(
                base::list(output = "", error = "unable to get active developer directory",
                           status = 2L),
                class = c("xcodeselect", "cli")
            )
        }
    )
    mockery::stub(developer_dir, "base::Sys.getenv", function(...) "")
    expect_true(base::is.na(developer_dir()))
})

test_that("xcode_cli_version reads the pkgutil receipt", {
    local_mocked_bindings(
        is_xcode_cli_installed = function() TRUE,
        exec_text = function(...) c(
            "package-id: com.apple.pkg.CLTools_Executables",
            "version: 27.0.0.0.1788430756",
            "volume: /"
        )
    )
    expect_equal(xcode_cli_version(), "27.0.0.0.1788430756")

    local_mocked_bindings(is_xcode_cli_installed = function() FALSE)
    expect_true(base::is.na(xcode_cli_version()))
})

test_that("xcode_app_bundles drops Time Machine hits and bundles without a developer dir", {
    local_mocked_bindings(
        exec_text = function(...) c(
            "/Applications/Xcode.app",
            "/Volumes/TM/Backups.backupdb/mac/2026-01-01/Applications/Xcode.app",
            "/Applications/Broken.app"
        )
    )
    mockery::stub(xcode_app_bundles, "base::Sys.glob", function(...) base::character(0))
    mockery::stub(xcode_app_bundles, "base::dir.exists",
                  function(path) !base::grepl("Broken", path))
    mockery::stub(xcode_app_bundles, "base::normalizePath", function(path, ...) path)

    expect_equal(xcode_app_bundles(), "/Applications/Xcode.app")
})

test_that("is_xcode_toolchain_usable requires an executable compiler, not just exit 0", {
    # xcrun can exit 0 while printing an error for a degraded bundle, so the
    # returned path must be confirmed to exist and be executable.
    local_mocked_bindings(
        exec_text = function(...) "/Applications/Xcode.app/Contents/Developer/usr/bin/clang"
    )
    mockery::stub(is_xcode_toolchain_usable, "base::file.exists", function(path) TRUE)
    mockery::stub(is_xcode_toolchain_usable, "base::file.access", function(...) 0L)
    expect_true(is_xcode_toolchain_usable())

    mockery::stub(is_xcode_toolchain_usable, "base::file.exists", function(path) FALSE)
    expect_false(is_xcode_toolchain_usable())
})

test_that("is_xcode_toolchain_usable is FALSE when xcrun fails", {
    local_mocked_bindings(exec_text = function(..., fallback = NA_character_) NA_character_)
    expect_false(is_xcode_toolchain_usable())
})

test_that("xcode_toolchains lists CLT and every Xcode bundle, flagging the active one", {
    local_mocked_bindings(
        assert_mac = function() TRUE,
        is_xcode_cli_installed = function() TRUE,
        xcode_cli_version = function() "27.0.0.0.1788430756",
        install_directory_xcode_cli = function() "/Library/Developer/CommandLineTools",
        xcode_app_bundles = function() c("/Applications/Xcode.app", "/Applications/Xcode_16.4.app"),
        xcode_app_version = function(bundle) if (base::grepl("16.4", bundle)) "16.4" else "27.0",
        developer_dir = function() "/Applications/Xcode_16.4.app/Contents/Developer"
    )

    out <- xcode_toolchains()

    expect_s3_class(out, "data.frame")
    expect_equal(base::nrow(out), 3L)
    expect_equal(base::names(out), c("path", "type", "version", "active"))
    expect_equal(base::sum(out$active), 1L)
    expect_equal(out$path[out$active], "/Applications/Xcode_16.4.app/Contents/Developer")
    expect_true("cli" %in% out$type)
    expect_equal(out$version[out$type == "cli"], "27.0.0.0.1788430756")
})

test_that("xcode_toolchains returns a zero-row frame when nothing is installed", {
    local_mocked_bindings(
        assert_mac = function() TRUE,
        is_xcode_cli_installed = function() FALSE,
        xcode_app_bundles = function() base::character(0),
        developer_dir = function() NA_character_
    )

    out <- xcode_toolchains()
    expect_s3_class(out, "data.frame")
    expect_equal(base::nrow(out), 0L)
    expect_equal(base::names(out), c("path", "type", "version", "active"))
})

test_that("xcode_cli_switch rejects a target that is not a developer directory", {
    local_mocked_bindings(
        assert_mac = function() TRUE,
        shell_execute = function(...) stop("shell_execute must not run for an invalid target")
    )
    mockery::stub(xcode_cli_switch, "base::dir.exists", function(path) FALSE)

    expect_error(
        xcode_cli_switch("/Applications/NotXcode.app", verbose = FALSE),
        regexp = "not a developer directory"
    )
})

test_that("xcode_cli_switch is a no-op when the target is already the stored selection", {
    local_mocked_bindings(
        assert_mac = function() TRUE,
        xcode_select_path = function() {
            base::structure(
                base::list(output = "/Library/Developer/CommandLineTools", error = "", status = 0L),
                class = c("xcodeselect", "cli")
            )
        },
        shell_execute = function(...) stop("shell_execute must not run for a no-op switch")
    )
    mockery::stub(xcode_cli_switch, "base::dir.exists", function(path) TRUE)

    expect_true(xcode_cli_switch("/Library/Developer/CommandLineTools", verbose = FALSE))
})

test_that("xcode_cli_switch still switches when only DEVELOPER_DIR matches the target", {
    # DEVELOPER_DIR overrides xcrun and the shims, but --switch writes the
    # STORED selection. Treating the override as "already active" would skip a
    # switch that never happened, and unsetting the variable would revert.
    commands <- base::character(0)
    local_mocked_bindings(
        assert_mac = function() TRUE,
        developer_dir = function() "/Library/Developer/CommandLineTools",
        xcode_select_path = function() {
            base::structure(
                base::list(output = "/Applications/Xcode.app/Contents/Developer", error = "", status = 0L),
                class = c("xcodeselect", "cli")
            )
        },
        shell_execute = function(cmd, ...) {
            commands <<- c(commands, cmd)
            0L
        }
    )
    mockery::stub(xcode_cli_switch, "base::dir.exists", function(path) TRUE)

    expect_true(xcode_cli_switch("/Library/Developer/CommandLineTools", verbose = FALSE))
    expect_length(commands, 1L)
})

test_that("xcode_cli_switch does not echo its path argument, which may be a password", {
    # `path` took `password`'s position, so a legacy positional call arrives
    # here carrying a sudo password. It must never reach the error message.
    local_mocked_bindings(
        assert_mac = function() TRUE,
        shell_execute = function(...) stop("must not run for an invalid target")
    )
    mockery::stub(xcode_cli_switch, "base::dir.exists", function(path) FALSE)

    err <- base::tryCatch(
        xcode_cli_switch("sup3rs3cret", verbose = FALSE),
        error = function(e) base::conditionMessage(e)
    )
    expect_no_match(err, "sup3rs3cret", fixed = TRUE)
    expect_match(err, "not a developer directory", fixed = TRUE)
})

test_that("xcode_cli_switch rejects an existing directory that is not a developer dir", {
    local_mocked_bindings(
        assert_mac = function() TRUE,
        shell_execute = function(...) stop("must not run for an invalid target")
    )
    # The directory exists, but has no usr/bin beneath it.
    mockery::stub(xcode_cli_switch, "base::dir.exists",
                  function(path) !base::grepl("usr/bin", path, fixed = TRUE))

    expect_error(xcode_cli_switch("/tmp", verbose = FALSE), "not a developer directory")
})

test_that("xcode_cli_switch accepts an explicit non-CLT target", {
    commands <- base::character(0)
    local_mocked_bindings(
        assert_mac = function() TRUE,
        xcode_select_path = function() {
            base::structure(
                base::list(output = "/Library/Developer/CommandLineTools", error = "", status = 0L),
                class = c("xcodeselect", "cli")
            )
        },
        shell_execute = function(cmd, ...) {
            commands <<- c(commands, cmd)
            0L
        }
    )
    mockery::stub(xcode_cli_switch, "base::dir.exists", function(path) TRUE)

    expect_true(xcode_cli_switch("/Applications/Xcode_16.4.app", verbose = FALSE))
    expect_length(commands, 1L)
    # The target is shell-quoted so a path containing spaces cannot split.
    expect_match(
        commands[1],
        base::paste("--switch", base::shQuote("/Applications/Xcode_16.4.app/Contents/Developer")),
        fixed = TRUE
    )
})

test_that("xcode_cli_switch quotes a target containing spaces", {
    commands <- base::character(0)
    local_mocked_bindings(
        assert_mac = function() TRUE,
        xcode_select_path = function() {
            base::structure(
                base::list(output = "/Library/Developer/CommandLineTools", error = "", status = 0L),
                class = c("xcodeselect", "cli")
            )
        },
        shell_execute = function(cmd, ...) {
            commands <<- c(commands, cmd)
            0L
        }
    )
    mockery::stub(xcode_cli_switch, "base::dir.exists", function(path) TRUE)
    mockery::stub(xcode_cli_switch, "base::normalizePath", function(path, ...) path)

    xcode_cli_switch("/Volumes/My Disk/Xcode.app", verbose = FALSE)

    expect_match(commands[1], "'/Volumes/My Disk/Xcode.app/Contents/Developer'", fixed = TRUE)
})

test_that("xcode_cli_uninstall never derives its removal target from the selection", {
    # Safety invariant: the rm target is a literal. A non-standard or versioned
    # Xcode selection must never appear in a destructive command.
    commands <- base::character(0)
    local_mocked_bindings(
        assert_mac = function() TRUE,
        is_xcode_cli_installed = function() TRUE,
        developer_dir = function() "/Applications/Xcode_16.4.app/Contents/Developer",
        shell_execute = function(cmd, ...) {
            commands <<- c(commands, cmd)
            0L
        }
    )

    xcode_cli_uninstall(verbose = FALSE)

    expect_length(commands, 1L)
    expect_equal(commands[1], "rm -rf /Library/Developer/CommandLineTools")
    expect_false(base::any(base::grepl("Xcode_16.4", commands)))
})

test_that("xcode_cli_uninstall reports not-installed rather than success", {
    local_mocked_bindings(
        assert_mac = function() TRUE,
        is_xcode_cli_installed = function() FALSE,
        shell_execute = function(...) stop("must not run when CLT is absent")
    )
    # FALSE means "nothing was removed", so callers cannot report success.
    expect_false(xcode_cli_uninstall(verbose = FALSE))
})
