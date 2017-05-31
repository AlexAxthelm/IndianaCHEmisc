context("check_elapsed_time")

test_that("check_elapsed_time handles missing arguments", {
  current_time <- Sys.time()
  Sys.sleep(0.1)
  # No arguments, so no comparisons
  expect_silent(check_elapsed_time())
  # Compares to the "last time" arguement. Note the use of regex
  expect_output(check_elapsed_time(last_time = current_time), "[[:digit:]]+[\\.]{0,1}[[:digit:]]{0,2} (secs|mins|hours|days) since last check")


  # Compares to the "start time" arguement. Note the use of regex
  expect_output(check_elapsed_time(start_time = current_time), "[[:digit:]]+[\\.]{0,1}[[:digit:]]{0,2} (secs|mins|hours|days) total")

  # No names on the arguement defaults to last_time
  # Compares to the "last time" arguement. Note the use of regex
  expect_output(check_elapsed_time(current_time), "[[:digit:]]+[\\.]{0,1}[[:digit:]]{0,2} (secs|mins|hours|days) since last check")

  # give a second time
  new_time <- Sys.time()
  Sys.sleep(.01)
  # Check that both outputs return
  expect_output(check_elapsed_time(last_time = new_time, start_time = current_time), "[[:digit:]]+[\\.]{0,1}[[:digit:]]{0,2} (secs|mins|hours|days) since last check, [[:digit:]]+[\\.]{0,1}[[:digit:]]{0,2} (secs|mins|hours|days) total")
})

test_that("Check_elapsed_time returns a POSIXct", {
  current_time <- Sys.time()
  Sys.sleep(0.1)
  # No arguments, so no comparisons
  expect_is(check_elapsed_time(), "POSIXct")
  # No named Arguments
  expect_is(check_elapsed_time(current_time), "POSIXct")
  expect_is(check_elapsed_time(last_time = current_time), "POSIXct")
  expect_is(check_elapsed_time(start_time = current_time), "POSIXct")

  expect_is(check_elapsed_time(last_time = NULL), "POSIXct")
  expect_is(check_elapsed_time(start_time = NULL), "POSIXct")

  new_time <- Sys.time()
  Sys.sleep(.01)
  expect_is(check_elapsed_time(last_time = new_time, start_time = current_time), "POSIXct")
})

test_that("check_elapsed_time can handle NULLs", {
  expect_silent(check_elapsed_time())
  expect_silent(check_elapsed_time(NULL))
  expect_silent(check_elapsed_time(last_time = NULL))
  expect_silent(check_elapsed_time(start_time = NULL))
  expect_silent(check_elapsed_time(last_time = NULL, start_time = NULL))
})

test_that("check_elapsed_time can handle NAs", {
  expect_silent(check_elapsed_time())
  expect_silent(check_elapsed_time(NA))
  expect_silent(check_elapsed_time(last_time = NA))
  expect_silent(check_elapsed_time(start_time = NA))
  expect_silent(check_elapsed_time(last_time = NA, start_time = NA))
})
