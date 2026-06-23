# Assert a condition

`assert()` checks a condition and throws an error with a formatted
message if it fails.

## Usage

``` r
assert(condition, message = NULL, call = caller_env())

assert_mac(call = caller_env())

assert_macos_supported(call = caller_env())

assert_aarch64(call = caller_env())

assert_x86_64(call = caller_env())

assert_r_version_supported(call = caller_env())
```

## Arguments

- condition:

  A logical indicating the status of the condition.

- message:

  A string to display in the error message.

- call:

  The calling environment (default: caller_env()).
