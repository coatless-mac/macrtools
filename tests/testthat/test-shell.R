test_that("shell_command executes commands correctly", {
    # Mock dependencies
    mockery::stub(shell_command, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(shell_command, "cli::cli_bullets", function(...) NULL)
    mockery::stub(shell_command, "cli::cli_text", function(...) NULL)
    mockery::stub(shell_command, "base::getwd", function() "/home/user")
    mockery::stub(shell_command, "base::system", function(cmd) {
        if (cmd == "echo test") return(0)
        return(1)
    })

    # Test successful command
    result <- shell_command("echo test", verbose = TRUE)
    expect_equal(result, 0)

    # Test failing command
    result <- shell_command("invalid_command", verbose = TRUE)
    expect_equal(result, 1)
})

test_that("shell_sudo_command executes privileged commands correctly", {
    # Mock dependencies
    mockery::stub(shell_sudo_command, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(shell_sudo_command, "cli::cli_bullets", function(...) NULL)
    mockery::stub(shell_sudo_command, "cli::cli_text", function(...) NULL)
    mockery::stub(shell_sudo_command, "cli::cli_alert_warning", function(...) NULL)
    mockery::stub(shell_sudo_command, "base::system", function(cmd, input) {
        if (grepl("echo test", cmd) && input == "password") return(0)
        return(1)
    })

    # Test successful command with password
    result <- shell_sudo_command("echo test", password = "password", verbose = TRUE)
    expect_equal(result, 0)

    # Test failing command
    result <- shell_sudo_command("invalid_command", password = "password", verbose = TRUE)
    expect_equal(result, 1)

    # Test with askpass
    mockery::stub(shell_sudo_command, "askpass::askpass", function(...) "password")
    result <- shell_sudo_command("echo test", password = NULL, verbose = TRUE)
    expect_equal(result, 0)
})

test_that("shell_execute routes commands correctly", {
    # Mock dependencies
    mockery::stub(shell_execute, "cli::cli_alert_info", function(...) NULL)
    mockery::stub(shell_execute, "cli::cli_code", function(...) NULL)
    mockery::stub(shell_execute, "cli::cli_text", function(...) NULL)
    mockery::stub(shell_execute, "cli::cli_alert_success", function(...) NULL)
    mockery::stub(shell_execute, "cli::cli_alert_warning", function(...) NULL)
    mockery::stub(shell_execute, "cli::cli_bullets", function(...) NULL)

    # Test regular command
    mockery::stub(shell_execute, "shell_command", function(cmd, verbose) {
        if (cmd == "echo test") return(0)
        return(1)
    })
    mockery::stub(shell_execute, "shell_sudo_command", function(cmd, password, verbose) 1)
    mockery::stub(shell_execute, "base::Sys.time", function() Sys.time())
    mockery::stub(shell_execute, "base::difftime", function(...) 1)
    mockery::stub(shell_execute, "base::round", function(...) 1)
    mockery::stub(shell_execute, "base::as.numeric", function(...) 1)

    result <- shell_execute("echo test", sudo = FALSE, verbose = TRUE)
    expect_equal(result, 0)

    # Test sudo command
    mockery::stub(shell_execute, "shell_command", function(cmd, verbose) 1)
    mockery::stub(shell_execute, "shell_sudo_command", function(cmd, password, verbose) {
        if (cmd == "echo test" && password == "password") return(0)
        return(1)
    })

    result <- shell_execute("echo test", sudo = TRUE, password = "password", verbose = TRUE)
    expect_equal(result, 0)
})
