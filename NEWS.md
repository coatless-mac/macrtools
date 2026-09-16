# macrtools 0.0.9

## Bug Fixes

- Fixed Xcode toolchain detection when the tools are not in the default
  location. `is_xcode_cli_installed()` and `is_xcode_app_installed()` compared
  the active developer directory against a single hardcoded path with
  `identical()`, so both returned `FALSE` whenever anything else was selected,
  even with a perfectly usable toolchain installed:

  ```r
  is_xcode_cli_installed()
  #> [1] FALSE
  xcode_cli_path()
  #> [1] "/Applications/Xcode_16.4.app/Contents/Developer"
  ```

  A versioned Xcode bundle like that is the normal multi-Xcode layout, and it
  is what every GitHub-hosted macOS runner selects, so this affected the
  package's own CI. `macos_rtools_install()` treated the two predicates as
  exhaustive and would start a 10-15 minute `softwareupdate` install of Command
  Line Tools that were already present.

- `xcode_cli_uninstall()` now returns `FALSE` when the Command Line Tools were
  not installed, instead of `TRUE`. `macos_rtools_uninstall()` reported "Xcode
  CLI: Successfully removed" on that path even though nothing had been removed.
  The summary now distinguishes a removal from a component that was absent.

- `macos_rtools_install()` no longer reports the output of
  `xcode-select --version` as the Command Line Tools version. That command
  reports the version of `xcode-select` itself and never changes with the
  toolchain. The version now comes from the package receipt.

## Features

- Added `xcode_toolchains()`, which lists every developer directory
  `xcode-select` could be pointed at: the Command Line Tools plus each
  installed Xcode bundle, with its version and whether it is active.

  ```r
  xcode_toolchains()
  #>                                         path type             version active
  #> 1 /Applications/Xcode.app/Contents/Developer  app                27.0   TRUE
  #> 2        /Library/Developer/CommandLineTools  cli 27.0.0.0.1788430756  FALSE
  ```

  Xcode bundles are found by bundle identifier through Spotlight, so a renamed
  or relocated install is still detected. A `/Applications/Xcode*.app` glob
  covers machines where Spotlight indexing is disabled, which catches the
  versioned names but not installs outside `/Applications`.
  This lists developer directories, not `.xctoolchain` bundles, which are a
  separate axis selected via `xcrun --toolchain` and `TOOLCHAINS`.

- Added `is_xcode_toolchain_usable()`, which reports whether a compiler is
  actually reachable via `xcrun --find clang`. This is the condition that
  determines whether R can build a package, since R is configured with a bare
  `clang` that resolves through the active developer directory. It is now what
  gates the install step.

- Added `developer_dir()`, which resolves the active developer directory and
  honors `DEVELOPER_DIR`. That variable overrides the `xcode-select` selection
  for `xcrun`, the `/usr/bin` shims and `xcodebuild`, and none of the package's
  detection accounted for it.

- Added `xcode_cli_version()`, reading the real Command Line Tools version from
  the `com.apple.pkg.CLTools_Executables` receipt.

- `xcode_cli_switch()` now takes a `path`, so a specific toolchain from
  `xcode_toolchains()` can be selected rather than always forcing the Command
  Line Tools. The target is validated before use, the displaced selection is
  named, and a set `DEVELOPER_DIR` is flagged because it silently overrides the
  stored selection.

- `macos_rtools_install()` now reports which toolchain is active rather than
  assuming it is a full Xcode at `/Applications/Xcode.app`, and reports a
  failure, instead of "no action needed", when the Command Line Tools are on
  disk but the active developer directory is stale so nothing can compile. It
  also verifies that a compiler is reachable *after* installing, rather than
  trusting the installer's return value, since a stale selection or a
  `DEVELOPER_DIR` override still shadows freshly installed tools.

- `xcode_cli_install()` no longer skips the installation merely because an
  Xcode bundle exists somewhere on disk. An unselected Xcode provides no
  compiler, so the skip now requires a toolchain that is actually active.

## Breaking Changes

- `is_xcode_cli_installed()` now reports whether the Command Line Tools
  **package** is installed, rather than whether it is the selected developer
  directory. Selection is a separate question, answered by `developer_dir()`
  and `is_xcode_toolchain_usable()`. The split is deliberate: the destructive
  path in `xcode_cli_uninstall()` must stay keyed on package presence, because
  widening it to mean "a usable toolchain is selected" would let a machine with
  Xcode selected delete the Command Line Tools.

- `is_xcode_app_installed()` now reports whether any Xcode bundle is installed
  anywhere, rather than whether `/Applications/Xcode.app` is the selected
  developer directory.

- `xcode_cli_switch()` takes `path` as its first argument, where `password`
  used to be. A positional call such as `xcode_cli_switch(pw)` now fails with a
  clear error rather than switching.

## Internal

- Path comparisons are normalized through `normalize_developer_dir()`, which
  expands an `.app` bundle to its developer directory, strips trailing slashes
  and resolves symlinks. macOS reports these paths in forms that `identical()`
  would never match, for instance `/tmp` against `/private/tmp`.
- Added tests covering non-standard, versioned and relocated toolchain layouts,
  plus a test pinning the invariant that `xcode_cli_uninstall()` never derives
  its removal target from the active selection.
- The `xcode-select --switch` target is shell-quoted, so the caller-supplied
  `path` cannot split on spaces. `xcode_cli_switch()` also declines to echo
  that argument in its error message, because it took `password`'s position and
  a legacy positional call would otherwise print a sudo password.

# macrtools 0.0.8

## Features

- Added support for macOS Golden Gate (27.x).

## Bug Fixes

- Fixed the macOS version check so that macOS 27 is recognized as supported.
  `is_macos_r_supported()` compares against an *exclusive* upper bound, so the
  previous value of `"27.0"` rejected every macOS 27 release, including 27.0
  itself. On macOS 27 this aborted `macos_rtools_install()`, `gfortran_install()`
  and `openmp_install()`, and printed an "unsupported" warning on attach.
  This is the same off-by-one that affected Tahoe
  ([#28](https://github.com/coatless-mac/macrtools/issues/28)).

- Fixed macOS version comparisons for `.0` releases. `sw_vers -productVersion`
  reports only two components for a `.0` release (macOS 27.0 reports `"27.0"`,
  never `"27.0.0"`), and `utils::compareVersion()` ranks a shorter string below
  an otherwise-equal longer one. Comparing those against three-component bounds
  placed every `.0` release in the *previous* release's range: `"12.0"` was
  reported as Big Sur rather than Monterey, `"11.0"` matched nothing at all, and
  `"10.13"` was rejected as unsupported by an error message that named 10.13 as
  supported. Versions and bounds are now padded to three components before
  comparison.

## Internal

- Centralized the macOS support window behind `minimum_supported_macos_version()`
  and `first_unsupported_macos_version()`, mirroring the R version window added
  in 0.0.7. The ceiling is now named for what it is, the first *rejected*
  version, which is the distinction both previous off-by-one bugs got wrong.
- The supported range shown by `assert_macos_supported()` and the startup
  message is now rendered by a single `macos_support_range()` helper that
  derives the version number from the bound. Previously the range was written
  out as a literal in two files, with different wording, and neither was tested.
- Added upper-bound tests for `is_macos_r_supported()`. The suite previously
  exercised only the lower bound, which is why both off-by-one bugs shipped
  without a test failure. Added a test asserting that exactly one named
  `is_macos_*()` predicate matches any given release, so a new release cannot be
  swallowed by its predecessor's range.
- Added `is_macos_golden_gate()` for 27.x.
- Added `pad_version()` to normalize version strings before comparison, and
  extended the predicate test to cover the two-component `.0` form that
  `sw_vers` actually emits.
- Corrected the `recipes_binary_install()` documentation, which stated that the
  `darwin` repository is chosen by comparing against the macOS version. It is
  chosen by comparing against the Darwin kernel version, and the two are no
  longer a fixed offset apart: Darwin stayed on 25 for macOS 26, then realigned
  to 27 for macOS 27, skipping 26.
- Noted in `R/openmp.R` that Apple renumbered Apple clang from 17.0.0
  (`clang-1700.x`) to 21.0.0 (`clang-2100.x`) at Xcode 26.4, so Xcode 26.4 and
  newer extrapolate onto the LLVM OpenMP 19.1.5 row. Upstream documents that
  runtime only through Xcode 26.3, but 19.1.5 remains the newest build published
  on mac.r-project.org, so the selection is unchanged and still correct.
- Bumped the declared `testthat` minimum to 3.1.7, the version that introduced
  `local_mocked_bindings()`, which the suite already relies on throughout.

# macrtools 0.0.7

## Features

- Added support for R 4.6.0 on macOS.
  - `gfortran` continues to use the universal GNU Fortran 14.2 installer
    (unchanged from R 4.5).
  - On Apple Silicon, R 4.6 (macOS 14 Sonoma and higher) installs the
    `recipes` binaries from the new `darwin23/arm64` repository. The
    repository is detected automatically and installs to the same
    `/opt/R/arm64` prefix as before, so no path changes are required.
- Updated the bundled LLVM OpenMP runtime from 19.1.0 to 19.1.5 to match the
  current release on <https://mac.r-project.org/openmp/>. The 19.1.x runtime
  covers Apple clang 1700.x and up (Xcode 16.3 through Xcode 26.x, the R 4.6
  Apple Silicon toolchain), so existing systems remain correctly matched.

## Internal

- Centralized the supported R version window behind
  `minimum_supported_r_version()` / `maximum_supported_r_version()` and replaced
  the repeated `is_r_version("4.x") || ...` chains with range checks
  (`is_r_version_at_least()`, `is_r_version_supported()`). Supporting a future R
  release that keeps the existing toolchain is now a one-line change.
- `gfortran_update()` now requires "R 4.2 or above" (matching its documentation)
  rather than an explicit, hard-coded list of versions.

# macrtools 0.0.6.2

## Features

- Added support for macOS Tahoe (26.x).
- Fixed the macOS version check so Tahoe (26.x) is recognized as supported.
  ([#28](https://github.com/coatless-mac/macrtools/issues/28))

# macrtools 0.0.6.1

## Features

- Added `openmp_*()` to install the LLVM OpenMP library for macOS. ([#10](https://github.com/coatless-mac/macrtools/issues/10))
  - `openmp_install()` to install the OpenMP library and set flags in `~/.R/Makevars`.
  - `openmp_uninstall()` to uninstall the OpenMP library and set flags in `~/.R/Makevars`.
  - `openmp_test()` for detailed overview of OpenMP install.
  - `is_openmp_installed()` to check if the OpenMP library is installed.

## Bug Fixes

- Fixed `macrtools::macos_rtools_install()` erroring during the summary
  portion.

# macrtools 0.0.6

## Features

- Updated to support R 4.5.0 on macOS.
  - Downloads and sets up the latest version of gfortran 14.2

# macrtools 0.0.5

## Features

- Updated to support macOS Sequoia (15.0.0)

# macrtools 0.0.4

## Features

- Updated to support R 4.4.0 on macOS.

## Deployment

- GitHub Actions now tests the package on `release` and `next` versions of 
  macOS R's version.

# macrtools 0.0.3

## Breaking change

- We've loosened the requirement to have Xcode CLI installed. The package will
  now accept if you have the Xcode.app IDE installed. 
  - Note: The Xcode.app IDE requires significantly more space compared to
    Xcode CLI. We highly suggest that you install Xcode CLI.

## Features

- Added `is_xcode_app_installed()` to check to see if the full Xcode.app IDE
  is present.
- Added `xcodebuild()` to understand properties about the Xcode.app IDE 
  installation.

## Changes

- Updated the supported version of macOS to macOS Sonoma (14.0.0).
- Improved the error message when the version of macOS is not supported.
- `xcode_cli_install()` and `macos_rtools_install()` have been modified to
  skip the installation of Xcode CLI if the full Xcode.app IDE is detected.

## Documentation

- Switched describing the ARM Mac entries from M1 or M2 to M-series to 
  generalize with how Apple names software.
- Improved details regarding the paths being chosen for software installed.

## Deployment

- Updated the GitHub Actions for both R-CMD-check and pkgdown.

# macrtools 0.0.2

## Features

- Support the toolchain compilation requirements for 4.3.z series 
- Updated the gfortran binary to the universal v12.2 Fortran installer across both Intel and arm64 platforms.
   - The gfortran path placed into `~/.Renviron` is now `/opt/gfortran/bin/gfortran`.
- Recipes now supports the `darwin20/x86_64` version (macOS 11) for Intel. 
  - New installation directories for recipes downloads are given as:  

Name | Installation Location | Target
-- | -- | --
darwin17/x86_64 | /usr/local | macOS 10.13, Intel (x86_64)
darwin20/arm64 | /opt/R/arm64 | macOS 11, Apple M1 (arm64)
darwin20/x86_64 | /opt/R/x86_64 | macOS 11, Intel (x86_64)


# macrtools 0.0.1

[![Video: Installing and using the `{macrtools}` package to setup the R Compilation Toolchain for macOS](http://img.youtube.com/vi/_fckF0fefXQ/0.jpg)](https://www.youtube.com/watch?v=_fckF0fefXQ)

## Features 

- Automatic compiled code toolchain installation on macOS for R 4.0 - 4.2
  - In R, type into console: `macrtools::macos_rtools_install()`
- Install specific components of macOS compiled toolchain are available through:
  - Install Xcode CLI Tools with: `macrtools::xcode_cli_install()`
  - Install gfortran with: `macrtools::gfortran_install()`
  - Install precompiled binaries from the [`recipes` for macOS project](https://github.com/R-macos/recipes)
    with `macrtools::recipes_binary_install('r-base-dev')`
- Check if toolchains components are available:
  - Check if Xcode CLI Tools is installed with: `macrtools::is_xcode_cli_installed()`
  - Check if gfortran is installed with: `macrtools::is_gfortran_installed()`
- Uninstall toolchains components when they are no longer needed:
  - Uninstall Xcode CLI Tools with: `macrtools::xcode_cli_uninstall()`
  - Uninstall gfortran with: `macrtools::gfortran_uninstall()`
