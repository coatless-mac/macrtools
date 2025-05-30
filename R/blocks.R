# blocks has been adopted from usethis and rlang files:
# https://github.com/r-lib/usethis/blob/7c8e0049a1e40e6dcabbde069bb29576215a11b6/R/block.R
# https://github.com/r-lib/usethis/blob/e5f34414aa1c70be79f9ac7e2a8bd9d96b94593c/R/write.R
# https://github.com/r-lib/rlang/blob/1c76bb0f9980731b2fdfab0029c5ee38ae586a24/R/vec.R
# usethis and rlang authors license the function under the MIT License.
#
# Light modifications have occurred to remove use of `ui_*()`

platform_line_ending = function() {
    if (base::.Platform$OS.type == "windows") "\r\n" else "\n"
}

read_utf8 = function(path, n = -1L) {
    base::readLines(path, n = n, encoding = "UTF-8", warn = FALSE)
}

write_utf8 = function(path, lines, append = FALSE, line_ending = NULL) {
    base::stopifnot(base::is.character(path))
    base::stopifnot(base::is.character(lines))

    file_mode = if (append) "ab" else "wb"
    con = base::file(path, open = file_mode, encoding = "utf-8")

    if (base::is.null(line_ending)) {
        line_ending = platform_line_ending()
    }

    # convert embedded newlines
    lines = base::gsub("\r?\n", line_ending, lines)
    base::writeLines(base::enc2utf8(lines), con, sep = line_ending, useBytes = TRUE)
    base::close(con)

    base::invisible(TRUE)
}

seq2 = function (from, to)
{
    if (base::length(from) != 1) {
        base::stop(base::sprintf("%s must be length one.", from))
    }
    if (base::length(to) != 1) {
        base::stop(base::sprintf("%s must be length one.", to))
    }
    if (from > to) {
        base::integer(0)
    }
    else {
        base::seq.int(from, to)
    }
}


block_append = function(desc, value, path,
                        block_start = "# <<<",
                        block_end = "# >>>",
                        block_prefix = NULL,
                        block_suffix = NULL,
                        sort = FALSE) {

    if (!base::is.null(path) && base::file.exists(path)) {
        lines = read_utf8(path)
        if (base::all(value %in% lines)) {
            return(FALSE)
        }

        block_lines = block_find(lines, block_start, block_end)
    } else {
        block_lines = NULL
    }

    base::message("Adding ", desc, " to ", path)

    if (base::is.null(block_lines)) {
        # changed as we have a cold start and want to enforce a block being present
        write_utf8(path, block_create(value, block_start, block_end), append = TRUE)
        return(TRUE)
    }


    start = block_lines[[1]]
    end = block_lines[[2]]
    block = lines[seq2(start, end)]

    new_lines = base::union(block, value)
    if (sort) {
        new_lines = base::sort(new_lines)
    }

    lines = base::c(
        lines[seq2(1, start - 1L)],
        new_lines,
        lines[seq2(end + 1L, base::length(lines))]
    )
    write_utf8(path, lines)

    TRUE
}

block_replace = function(desc, value, path,
                         block_start = "# <<<",
                         block_end = "# >>>") {
    if (!base::is.null(path) && base::file.exists(path)) {
        lines = read_utf8(path)
        block_lines = block_find(lines, block_start, block_end)
    } else {
        block_lines = NULL
    }

    if (base::is.null(block_lines)) {
        base::message("Copy and paste the following lines into ", path, ":")
        base::paste0(base::c(block_start, value, block_end), collapse = "\n")
        return(base::invisible(FALSE))
    }

    start = block_lines[[1]]
    end = block_lines[[2]]
    block = lines[seq2(start, end)]

    if (base::identical(value, block)) {
        return(base::invisible(FALSE))
    }

    base::message("Replacing ", desc, " in ", path)

    lines = base::c(
        lines[seq2(1, start - 1L)],
        value,
        lines[seq2(end + 1L, base::length(lines))]
    )
    write_utf8(path, lines)
}


block_show = function(path, block_start = "# <<<", block_end = "# >>>") {
    lines = read_utf8(path)
    block = block_find(lines, block_start, block_end)
    lines[seq2(block[[1]], block[[2]])]
}

block_find = function(lines, block_start = "# <<<", block_end = "# >>>") {
    # No file
    if (base::is.null(lines)) {
        return(NULL)
    }

    start = base::which(lines == block_start)
    end = base::which(lines == block_end)

    # No block
    if (base::length(start) == 0 && base::length(end) == 0) {
        return(NULL)
    }

    if (!(base::length(start) == 1 && base::length(end) == 1 && start < end)) {
        base::stop("Invalid block specification.")
    }

    base::c(start + 1L, end - 1L)
}

block_create = function(lines = character(), block_start = "# <<<", block_end = "# >>>") {
    base::c("\n", block_start, base::unique(lines), block_end)
}
