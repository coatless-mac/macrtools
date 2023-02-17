# Obtained from:
#
# https://github.com/R-macos/R-mac-web/blob/52bffebdfcb9df3ba9549c14a824df1e7b4b9586/cran/bin/install.R
# (C)2021-22 R Core Team, License: MIT, Author: Simon Urbanek
#
# Barely modified for formatting

#' @include shell.R utils.R installers.R
NULL

#' Install Binary Library from Recipes Project
#'
#' Convenience function that seeks to install pre-built binary libraries
#' used on [CRAN](https://cran.r-project.org/) for macOS through the
#' [recipes](https://github.com/s-u/recipes) system designed by Simon Urbanek.
#'
#' @param pkgs    Character vector of binary names to install, `"all"` for all binaries, or `"r-base-dev"` for _R_ binaries.
#' @param url     URL of the repository root. Default <https://mac.R-project.org/bin>
#' @param os      Name and version of the OS, e.g. `"darwin22"` where `"darwin"`
#'                refers to macOS and `22` is the kernel version number.
#' @param arch    The architecture of either `arm64` (M1/M2) or `x86_64` (Intel). This is only used if `os.arch="auto"`.
#' @param os.arch Either name of the repository such as `"darwin20/arm64"`, `"darwin17/x86_64"`, or `"auto"`.
#'                Default `"auto"`.
#' @param dependencies Install build dependencies (`TRUE`) or only the requested packages (`FALSE`). Default `TRUE`.
#' @param action  Determine if the binary should be downloaded and installed (`"install"`)
#'                or displayed (`"list"`). Default `"install"` to download and install the binaries.
#' @param sudo    Attempt to install the binaries using `sudo` permissions.
#'                Default `TRUE`.
#' @param password User password to switch into the `sudo` user. Default `NULL`.
#' @param verbose Describe the steps being taken. Default `TRUE`
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
#' | [darwin20/arm64](https://mac.r-project.org/bin/darwin20/arm64)   | /opt/R/arm64          | macOS 11, Apple M1 (arm64)         |
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
    os = tolower(
        paste0(
            system("uname -s", intern = TRUE),
            gsub("\\..*", "", system("uname -r", intern = TRUE))
        )
    ),
    arch = system("uname -m", intern = TRUE),
    os.arch = "auto",
    dependencies = TRUE,
    action = c("install", "list"),
    sudo = TRUE,
    password = NULL,
    verbose = TRUE) {

    up <- function(...)
        paste(..., sep = '/')
    action <- match.arg(action)

    if (os.arch == "auto") {
        rindex <- up(url, "REPOS")
        cat("Downloading", rindex, "...\n")
        rl <- readLines(u <- url(rindex))
        close(u)
        rla <- simplify2array(strsplit(rl[grep("/", rl)], "/"))
        rl <- data.frame(os = rla[1, ], arch = rla[2, ])

        os.name <- function(os)
            gsub("[0-9].*", "", os)
        os.ver <-
            function(os)
                as.numeric(sub("\\..*", "", gsub("[^0-9]*", "", os)))

        rl$os.name <- os.name(rl$os)
        rl$os.ver <- os.ver(rl$os)

        rl <-
            rl[rl$os.name == os.name(os) & rl$os.ver <= os.ver(os), ]
        if (nrow(rl) < 1)
            stop(
                "There is no repository that supports ",
                os.name(os),
                " version ",
                os.ver(os),
                " or higher.\nAvailable binaries only support: ",
                paste(rla[1, ], collapse = ", ")
            )

        if (!any(rl$arch == arch))
            stop(
                "Architecture ",
                arch,
                " is not supported on os ",
                os,
                ", only available architectures: ",
                rl$arch
            )

        rl <- rl[rl$arch == arch, ]
        rl <- rl[order(rl$os.ver, decreasing = TRUE), ][1, ]

        os.arch <- file.path(rl$os, rl$arch)
    }

    cat("Using repository ", up(url, os.arch), "...\n")

    deps <- function(pkgs, db) {
        ## convert bare (w/o version) names to full names
        bare <- pkgs %in% db[, "Package"]
        if (any(bare))
            pkgs[bare] = rownames(db)[match(pkgs[bare], db[, "Package"])]

        ## any missing?
        mis <- !pkgs %in% rownames(db)
        if (any(mis))
            stop("Following binaries have no download candidates: ",
                 paste(pkgs[mis], collapse = ", "))

        dep <- function(pkgs) {
            mis <- !pkgs %in% rownames(db)
            if (any(mis))
                stop(
                    "Following binaries have no download candidates: ",
                    paste(pkgs[mis], collapse = ", ")
                )

            nd <-
                stats::na.omit(unique(c(pkgs, unlist(
                    strsplit(db[pkgs, "BuiltWith"], "[, ]+")
                ))))
            if (length(unique(pkgs)) < length(nd))
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
    cat("Downloading index ", pindex, "...\n")
    db <- read.dcf(u <- url(pindex))
    close(u)
    rownames(db) <- if ("Bundle" %in% colnames(db))
        ifelse(is.na(db[, "Bundle"]),
               paste(db[, "Package"], db[, "Version"], sep = '-'),
               db[, "Bundle"])
    else
        paste(db[, "Package"], db[, "Version"], sep = '-')

    if (identical(pkgs, "all"))
        pkgs <- stats::na.omit(db[, "Package"])
    need <- deps(pkgs, db)
    ## remove bundles as they have no binary
    if ("Bundle" %in% colnames(db) &&
        any(rem <- need %in% stats::na.omit(db[, "Bundle"])))
        need <- need[!rem]
    urls <- up(url, os.arch, db[need, "Binary"])

    if (action == "install") {

        # Check if we're using sudo & have a password
        if (sudo && is.null(password))
            password = askpass::askpass()

        # Determine the correct installation path based on arch type
        supplied_arch = strsplit(os.arch, "/")[[1]][2]
        installation_directory = install_location(supplied_arch)
        installation_strip_level = install_strip_level(supplied_arch)

        for (binary_url in urls) {

            # Download tar
            path_to_tar = download_tar_package(binary_url)

            # Install tar into the appropriate location
            install_tar_package(path_to_tar,
                                installation_directory,
                                installation_strip_level,
                                sudo    = sudo,
                                password = password,
                                verbose = verbose)
        }
    } else
        urls
}

