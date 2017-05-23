context("check_elapsed_time")



test_that("check_elapsed_time handles missing arguments", {
  current_time <- proc.time()
  Sys.sleep(0.1)
  # No arguments, so no comparisons
  expect_silent(check_elapsed_time())
  # Compares to the "last time" arguement. Note the use of regex
  expect_message(check_elapsed_time(last_time = current_time), "[[:digit:]]+[\\.]{0,1}[[:digit:]]{0,2} Seconds since last check")


  # Compares to the "start time" arguement. Note the use of regex
  expect_message(check_elapsed_time(start_time = current_time), "[[:digit:]]+[\\.]{0,1}[[:digit:]]{0,2} Seconds total")

  # No names on the arguement defaults to last_time
  # Compares to the "last time" arguement. Note the use of regex
  expect_message(check_elapsed_time(current_time), "[[:digit:]]+[\\.]{0,1}[[:digit:]]{0,2} Seconds since last check")

  # give a second time
  new_time <- proc.time()
  Sys.sleep(.01)
  # Check that both messages return
  expect_message(check_elapsed_time(last_time = new_time, start_time = current_time), "[[:digit:]]+[\\.]{0,1}[[:digit:]]{0,2} Seconds since last check, [[:digit:]]+[\\.]{0,1}[[:digit:]]{0,2} Seconds total")
})

test_that("Check_elapsed_time returns a proc_time", {
  current_time <- proc.time()
  Sys.sleep(0.1)
  # No arguments, so no comparisons
  expect_is(check_elapsed_time(), "proc_time")
  # No named Arguments
  expect_is(check_elapsed_time(current_time), "proc_time")
  expect_is(check_elapsed_time(last_time = current_time), "proc_time")
  expect_is(check_elapsed_time(start_time = current_time), "proc_time")

  expect_is(check_elapsed_time(last_time = NULL), "proc_time")
  expect_is(check_elapsed_time(start_time = NULL), "proc_time")

  new_time <- proc.time()
  Sys.sleep(.01)
  expect_is(check_elapsed_time(last_time = new_time, start_time = current_time), "proc_time")
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



test_that("check that either a a full proc_time or just the 'elapsed' element work", {
  current_time <- proc.time()
  cte <- current_time["elapsed"]
  ct3 <- current_time[3]

  noname <- current_time
  names(noname) <- NULL
  nn3 <- noname[3]
  Sys.sleep(.1)

  #start actual tests
  expect_message(check_elapsed_time(last_time = current_time), "[[:digit:]]+[\\.]{0,1}[[:digit:]]{0,2} Seconds since last check")
  expect_message(check_elapsed_time(last_time = cte), "[[:digit:]]+[\\.]{0,1}[[:digit:]]{0,2} Seconds since last check")
  expect_message(check_elapsed_time(last_time = ct3), "[[:digit:]]+[\\.]{0,1}[[:digit:]]{0,2} Seconds since last check")

  expect_message(check_elapsed_time(start_time = current_time), "[[:digit:]]+[\\.]{0,1}[[:digit:]]{0,2} Seconds total")
  expect_message(check_elapsed_time(start_time = cte), "[[:digit:]]+[\\.]{0,1}[[:digit:]]{0,2} Seconds total")
  expect_message(check_elapsed_time(start_time = ct3), "[[:digit:]]+[\\.]{0,1}[[:digit:]]{0,2} Seconds total")

  expect_message(check_elapsed_time(last_time = current_time, start_time = current_time), "[[:digit:]]+[\\.]{0,1}[[:digit:]]{0,2} Seconds since last check")
  expect_message(check_elapsed_time(last_time = current_time, start_time = current_time), "[[:digit:]]+[\\.]{0,1}[[:digit:]]{0,2} Seconds total")

  expect_message(check_elapsed_time(last_time = current_time, start_time = cte), "[[:digit:]]+[\\.]{0,1}[[:digit:]]{0,2} Seconds since last check")
  expect_message(check_elapsed_time(last_time = current_time, start_time = cte), "[[:digit:]]+[\\.]{0,1}[[:digit:]]{0,2} Seconds total")

  expect_message(check_elapsed_time(last_time = current_time, start_time = ct3), "[[:digit:]]+[\\.]{0,1}[[:digit:]]{0,2} Seconds since last check")
  expect_message(check_elapsed_time(last_time = current_time, start_time = ct3), "[[:digit:]]+[\\.]{0,1}[[:digit:]]{0,2} Seconds total")

  expect_message(check_elapsed_time(last_time = cte, start_time = ct3), "[[:digit:]]+[\\.]{0,1}[[:digit:]]{0,2} Seconds since last check")
  expect_message(check_elapsed_time(last_time = cte, start_time = ct3), "[[:digit:]]+[\\.]{0,1}[[:digit:]]{0,2} Seconds total")

  # start with the nonames:
  expect_silent(check_elapsed_time(noname))
  expect_silent(check_elapsed_time(start_time = noname))
  expect_silent(check_elapsed_time(last_time = noname))
  expect_silent(check_elapsed_time(start_time = noname, last_time = noname))

  expect_silent(check_elapsed_time(start_time = nn3))
})
