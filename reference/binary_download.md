# Download Binary Packages

The *R* temporary directory is created using
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) that consults the
environment values of [TMPDIR](https://rdrr.io/r/base/EnvVar.html),
[TMP](https://mac.thecoatlessprofessor.com/macrtools/reference/TMPDIR),
or
[TEMP](https://mac.thecoatlessprofessor.com/macrtools/reference/TMPDIR)
to establish the path. We need to use a temporary directory as `root` is
not guaranteed to have access to user's files.

## Usage

``` r
binary_download(
  url,
  binary_file_name = base::basename(url),
  verbose = TRUE,
  mode = "wb",
  timeout = 600
)
```

## Arguments

- url:

  A link containing the binary file to download.

- binary_file_name:

  Name of the binary file to save. Default `basname(url)`

- verbose:

  Display a status messages. Default `true`

- mode:

  Mode to use for download. Default is "wb" (binary mode)

- timeout:

  Timeout in seconds. Default is 600 (10 minutes)

## Value

The file path for the binary file in the temporary *R* directory

## Details

Downloads a binary package onto a user's computer into the temporary
directory.
