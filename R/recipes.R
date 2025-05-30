# Obtained from:
#
# https://github.com/R-macos/R-mac-web/blob/52bffebdfcb9df3ba9549c14a824df1e7b4b9586/cran/bin/install.R
# (C)2021-22 R Core Team, License: MIT, Author: Simon Urbanek
#
# Barely modified for formatting

#' Install Binary Library from the CRAN R macOS Recipes Project
#'
#' Convenience function that seeks to install pre-built binary libraries
#' used on [CRAN](https://cran.r-project.org/) for macOS through the
#' [recipes](https://github.com/s-u/recipes) system designed by Simon Urbanek.
#'
#' @param pkgs    Character vector of binary names to install, `"all"` for all binaries, or `"r-base-dev"` for _R_ binaries.
#' @param url     URL of the repository root. Default <https://mac.R-project.org/bin>
#' @param os      Name and version of the OS, e.g. `"darwin22"` where `"darwin"`
#'                refers to macOS and `22` is the kernel version number.
#' @param arch    The architecture of either `arm64` (M1/M2/M3) or `x86_64` (Intel). This is only used if `os.arch="auto"`.
#' @param os.arch Either name of the repository such as `"darwin20/arm64"`, `"darwin20/x86_64"`, `"darwin17/x86_64"`, or `"auto"`.
#'                Default `"auto"`.
#' @param dependencies Install build dependencies (`TRUE`) or only the requested packages (`FALSE`). Default `TRUE`.
#' @param action  Determine if the binary should be downloaded and installed (`"install"`),
#'                displayed (`"list"`), or downloaded but not installed (`"download"`).
#'                Default `"install"` to download and install the binaries.
#' @param sudo    Attempt to install the binaries using `sudo` permissions.
#'                Default `TRUE`.
#' @param password User password to switch into the `sudo` user. Default `NULL`.
#' @param verbose Describe the steps being taken. Default `TRUE`.
#' @export
#' @author
#' Simon Urbanek wrote the function and made it available at
#' <https://mac.r-project.org/bin/>
#'
#' James Joseph Balamuta packaged the function and added the option to use
#' `sudo` on the command line.
#'
#' @details
#'
#' The function attempts to detect the appropriate repository and installation
#' path for the binary packages when `"auto"` is set. By default, the
#' repository and the install path are either:
#'
#' | Name                                                             | Installation Location | Target                             |
#' | ---------------------------------------------------------------- | --------------------- | ---------------------------------- |
#' | [darwin17/x86_64](https://mac.r-project.org/bin/darwin17/x86_64) | /usr/local            | macOS 10.13, Intel (x86_64)        |
#' | [darwin20/x86_64](https://mac.r-project.org/bin/darwin20/x86_64) | /opt/R/x86_64         | macOS 11, Intel (x86_64)           |
#' | [darwin20/arm64](https://mac.r-project.org/bin/darwin20/arm64)   | /opt/R/arm64          | macOS 11, Apple M1 (arm64)         |
#'
#' @section Differences:
#' The official implementation uses `quiet` as a parameter to suppress output
#' instead of `verbose`.
#'
#' @examples
#' # Perform a dry-run to see the required development packages.
#' recipes_binary_install("r-base-dev", action = "list")
#'
#' \dontrun{
#' # Install the mandatory library binaries for building R on macOS using sudo
#' recipes_binary_install("r-base-dev", sudo = TRUE)
#' }
recipes_binary_install = function(
        pkgs,
        url = "https://mac.R-project.org/bin",
        os = base::tolower(
            base::paste0(
                base::system("uname -s", intern = TRUE),
                base::gsub("\\..*", "", base::system("uname -r", intern = TRUE))
            )
        ),
        arch = base::system("uname -m", intern = TRUE),
        os.arch = "auto",
        dependencies = TRUE,
        action = c("install", "list", "download"),
        sudo = TRUE,
        password = NULL,
        verbose = TRUE) {

    up <- function(...)
        base::paste(..., sep = '/')
    action <- base::match.arg(action)

    if (os.arch == "auto") {
        rindex <- up(url, "REPOS")
        if (verbose) base::cat("Downloading", rindex, "...\n")
        rl <- base::readLines(u <- base::url(rindex))
        base::close(u)
        rla <- base::simplify2array(base::strsplit(rl[base::grep("/", rl)], "/"))
        rl <- base::data.frame(os = rla[1, ], arch = rla[2, ])

        os.name <- function(os)
            base::gsub("[0-9].*", "", os)
        os.ver <-
            function(os)
                base::as.numeric(base::sub("\\..*", "", base::gsub("[^0-9]*", "", os)))

        rl$os.name <- os.name(rl$os)
        rl$os.ver <- os.ver(rl$os)

        rl <-
            rl[rl$os.name == os.name(os) & rl$os.ver <= os.ver(os), ]
        if (base::nrow(rl) < 1)
            base::stop(
                "There is no repository that supports ",
                os.name(os),
                " version ",
                os.ver(os),
                " or higher.\nAvailable binaries only support: ",
                base::paste(rla[1, ], collapse = ", ")
            )

        if (!base::any(rl$arch == arch))
            base::stop(
                "Architecture ",
                arch,
                " is not supported on os ",
                os,
                ", only available architectures: ",
                rl$arch
            )

        rl <- rl[rl$arch == arch, ]
        rl <- rl[base::order(rl$os.ver, decreasing = TRUE), ][1, ]

        os.arch <- base::file.path(rl$os, rl$arch)
    }

    if (verbose) base::cat("Using repository ", up(url, os.arch), "...\n")

    deps <- function(pkgs, db) {
        ## convert bare (w/o version) names to full names
        bare <- pkgs %in% db[, "Package"]
        if (base::any(bare))
            pkgs[bare] = base::rownames(db)[base::match(pkgs[bare], db[, "Package"])]

        ## any missing?
        mis <- !pkgs %in% base::rownames(db)
        if (base::any(mis))
            base::stop("Following binaries have no download candidates: ",
                       base::paste(pkgs[mis], collapse = ", "))

        dep <- function(pkgs) {
            mis <- !pkgs %in% base::rownames(db)
            if (base::any(mis))
                base::stop(
                    "Following binaries have no download candidates: ",
                    base::paste(pkgs[mis], collapse = ", ")
                )

            nd <-
                stats::na.omit(base::unique(base::c(pkgs, base::unlist(
                    base::strsplit(db[pkgs, "BuiltWith"], "[, ]+")
                ))))
            if (base::length(base::unique(pkgs)) < base::length(nd))
                dep(nd)
            else
                nd
        }
        if (dependencies)
            dep(pkgs)
        else
            pkgs
    }

    pindex <- up(url, os.arch, "PACKAGES")
    if (verbose) base::cat("Downloading index ", pindex, "...\n")
    db <- base::read.dcf(u <- base::url(pindex))
    base::close(u)
    base::rownames(db) <- if ("Bundle" %in% base::colnames(db))
        base::ifelse(base::is.na(db[, "Bundle"]),
                     base::paste(db[, "Package"], db[, "Version"], sep = '-'),
                     db[, "Bundle"])
    else
        base::paste(db[, "Package"], db[, "Version"], sep = '-')

    if (base::identical(pkgs, "all"))
        pkgs <- stats::na.omit(db[, "Package"])
    need <- deps(pkgs, db)
    ## remove bundles as they have no binary
    if ("Bundle" %in% base::colnames(db) &&
        base::any(rem <- need %in% stats::na.omit(db[, "Bundle"])))
        need <- need[!rem]
    urls <- up(url, os.arch, db[need, "Binary"])

    if (action == "install") {

        # Check if we're using sudo & have a password
        entered_recipes_password = password
        if (sudo && base::is.null(entered_recipes_password))
            entered_recipes_password = askpass::askpass()

        # Determine the correct installation path based on arch type
        supplied_arch = base::strsplit(os.arch, "/")[[1]][2]
        installation_directory = recipe_binary_install_location(supplied_arch)
        installation_strip_level = recipe_binary_install_strip_level(supplied_arch)

        # Ensure the installation location is valid.
        create_install_location(arch = supplied_arch, password = entered_recipes_password)

        for (binary_url in urls) {

            # Download tar
            path_to_tar = binary_download(binary_url, verbose = verbose)

            # Install tar into the appropriate location
            tar_package_install(path_to_tar,
                                installation_directory,
                                installation_strip_level,
                                sudo    = sudo,
                                password = entered_recipes_password,
                                verbose = verbose)
        }

        return(base::invisible(TRUE))
    } else if (action == "download") {
        for (u in urls) {
            if (!base::file.exists(base::basename(u))) {

            if (verbose) base::cat("Downloading ", u, "...\n", sep='')

            if (base::system(base::paste("curl", "-sSLO", base::shQuote(u))) < 0)
                base::stop("Failed to download ", u)
            }
        }
    } else
        urls
}
