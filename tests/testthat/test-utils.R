test_that("check R version", {

    # Check current version
    minor_value = strsplit(R.version$minor, ".", fixed = TRUE)[[1]][1]
    version_string = paste(R.version$major, minor_value, sep = ".")

    # Check minor release
    expect_true(is_r_version(version_string))

    # Check full release
    version_string = paste(R.version$major, R.version$minor, sep=".")
    expect_true(is_r_version(version_string, compare_major_minor = FALSE))

    # Check old version not in test grid
    expect_false(is_r_version("3.6"))

    # Check patch release out of test grid
    expect_false(is_r_version("3.6.1", compare_major_minor = FALSE))
})
