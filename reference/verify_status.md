# Verify Status of Operation

Verify Status of Operation

## Usage

``` r
verify_status(status, program, url, type = c("uninstall", "install"))
```

## Arguments

- status:

  Status code from operation

- program:

  Name of the program being installed or uninstalled

- url:

  Optional URL for manual instructions

- type:

  Type of operation ("uninstall" or "install")

## Value

TRUE if status is successful, FALSE otherwise (invisibly)
