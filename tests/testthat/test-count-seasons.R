context("Count Seasons")

test_that("Season Difference - Start Spring", {
  # These were calculated by hand, by Alex Axthelm,
  # by counting the seasons in between each term
  # Start in spring
  expect_equal(count_seasons(20023, 20023, ignore_summers = FALSE), 0)
  # Summer 1
  expect_equal(count_seasons(20023, 20024, ignore_summers = FALSE), 1)
  # Summer 2
  expect_equal(count_seasons(20023, 20031, ignore_summers = FALSE), 1)
  # Fall
  expect_equal(count_seasons(20023, 20032, ignore_summers = FALSE), 2)
  # Next Spring
  expect_equal(count_seasons(20023, 20033, ignore_summers = FALSE), 3)
  expect_equal(count_seasons(20023, 20034, ignore_summers = FALSE), 4)
  expect_equal(count_seasons(20023, 20042, ignore_summers = FALSE), 5)
  expect_equal(count_seasons(20023, 20043, ignore_summers = FALSE), 6)
  expect_equal(count_seasons(20023, 20044, ignore_summers = FALSE), 7)
  expect_equal(count_seasons(20023, 20052, ignore_summers = FALSE), 8)
  expect_equal(count_seasons(20023, 20053, ignore_summers = FALSE), 9)
  expect_equal(count_seasons(20023, 20054, ignore_summers = FALSE), 10)
  expect_equal(count_seasons(20023, 20062, ignore_summers = FALSE), 11)
})

test_that("Season Difference - Start Spring - No Summers", {
  # These were calculated by hand, by Alex Axthelm
  # by counting the seasons in between each term
  # Start in spring
  expect_equal(count_seasons(20023, 20023, ignore_summers = TRUE), 0)
  # Summer 1
  expect_equal(count_seasons(20023, 20024, ignore_summers = TRUE), 0)
  # Summer 2
  expect_equal(count_seasons(20023, 20031, ignore_summers = TRUE), 0)
  # Fall
  expect_equal(count_seasons(20023, 20032, ignore_summers = TRUE), 1)
  # Next Spring
  expect_equal(count_seasons(20023, 20033, ignore_summers = TRUE), 2)
  expect_equal(count_seasons(20023, 20034, ignore_summers = TRUE), 2)
  expect_equal(count_seasons(20023, 20042, ignore_summers = TRUE), 3)
  expect_equal(count_seasons(20023, 20043, ignore_summers = TRUE), 4)
  expect_equal(count_seasons(20023, 20044, ignore_summers = TRUE), 4)
  expect_equal(count_seasons(20023, 20052, ignore_summers = TRUE), 5)
  expect_equal(count_seasons(20023, 20053, ignore_summers = TRUE), 6)
  expect_equal(count_seasons(20023, 20054, ignore_summers = TRUE), 6)
  expect_equal(count_seasons(20023, 20062, ignore_summers = TRUE), 7)
})

test_that("Season Difference - Start Summer", {
  # These were calculated by hand, by Alex Axthelm,
  # by counting the seasons in between each term
  # Start in spring
  # Summer 1
  expect_equal(count_seasons(20024, 20024, ignore_summers = FALSE), 0)
  # Summer 2
  expect_equal(count_seasons(20024, 20031, ignore_summers = FALSE), 0)
  # Fall
  expect_equal(count_seasons(20024, 20032, ignore_summers = FALSE), 1)
  # Next Spring
  expect_equal(count_seasons(20024, 20033, ignore_summers = FALSE), 2)
  expect_equal(count_seasons(20024, 20034, ignore_summers = FALSE), 3)
  expect_equal(count_seasons(20024, 20042, ignore_summers = FALSE), 4)
  expect_equal(count_seasons(20024, 20043, ignore_summers = FALSE), 5)
  expect_equal(count_seasons(20024, 20044, ignore_summers = FALSE), 6)
  expect_equal(count_seasons(20024, 20052, ignore_summers = FALSE), 7)
  expect_equal(count_seasons(20024, 20053, ignore_summers = FALSE), 8)
  expect_equal(count_seasons(20024, 20054, ignore_summers = FALSE), 9)
  expect_equal(count_seasons(20024, 20062, ignore_summers = FALSE), 10)
})

test_that("Season Difference - Start Summer - No Summers", {
  # These were calculated by hand, by Alex Axthelm
  # by counting the seasons in between each term
  # Start in spring
  # Summer 1
  expect_equal(count_seasons(20024, 20024, ignore_summers = TRUE), 0)
  # Summer 2
  expect_equal(count_seasons(20024, 20031, ignore_summers = TRUE), 0)
  # Fall
  expect_equal(count_seasons(20024, 20032, ignore_summers = TRUE), 1)
  # Next Spring
  expect_equal(count_seasons(20024, 20033, ignore_summers = TRUE), 2)
  expect_equal(count_seasons(20024, 20034, ignore_summers = TRUE), 2)
  expect_equal(count_seasons(20024, 20042, ignore_summers = TRUE), 3)
  expect_equal(count_seasons(20024, 20043, ignore_summers = TRUE), 4)
  expect_equal(count_seasons(20024, 20044, ignore_summers = TRUE), 4)
  expect_equal(count_seasons(20024, 20052, ignore_summers = TRUE), 5)
  expect_equal(count_seasons(20024, 20053, ignore_summers = TRUE), 6)
  expect_equal(count_seasons(20024, 20054, ignore_summers = TRUE), 6)
  expect_equal(count_seasons(20024, 20062, ignore_summers = TRUE), 7)
})

test_that("Season Difference - Start Fall", {
  # These were calculated by hand, by Alex Axthelm,
  # by counting the seasons in between each term
  # Start in spring
  # Fall
  expect_equal(count_seasons(20032, 20032, ignore_summers = FALSE), 0)
  # Next Spring
  expect_equal(count_seasons(20032, 20033, ignore_summers = FALSE), 1)
  # Summer 1
  expect_equal(count_seasons(20032, 20034, ignore_summers = FALSE), 2)
  # Summer 2
  expect_equal(count_seasons(20032, 20041, ignore_summers = FALSE), 2)
  expect_equal(count_seasons(20032, 20042, ignore_summers = FALSE), 3)
  expect_equal(count_seasons(20032, 20043, ignore_summers = FALSE), 4)
  expect_equal(count_seasons(20032, 20044, ignore_summers = FALSE), 5)
  expect_equal(count_seasons(20032, 20052, ignore_summers = FALSE), 6)
  expect_equal(count_seasons(20032, 20053, ignore_summers = FALSE), 7)
  expect_equal(count_seasons(20032, 20054, ignore_summers = FALSE), 8)
  expect_equal(count_seasons(20032, 20062, ignore_summers = FALSE), 9)
})

test_that("Season Difference - Start Fall - No Summers", {
  # These were calculated by hand, by Alex Axthelm
  # by counting the seasons in between each term
  # Start in spring
  # Fall
  expect_equal(count_seasons(20032, 20032, ignore_summers = TRUE), 0)
  # Next Spring
  expect_equal(count_seasons(20032, 20033, ignore_summers = TRUE), 1)
  # Summer 1
  expect_equal(count_seasons(20032, 20034, ignore_summers = TRUE), 1)
  # Summer 2
  expect_equal(count_seasons(20032, 20041, ignore_summers = TRUE), 1)
  expect_equal(count_seasons(20032, 20042, ignore_summers = TRUE), 2)
  expect_equal(count_seasons(20032, 20043, ignore_summers = TRUE), 3)
  expect_equal(count_seasons(20032, 20044, ignore_summers = TRUE), 3)
  expect_equal(count_seasons(20032, 20052, ignore_summers = TRUE), 4)
  expect_equal(count_seasons(20032, 20053, ignore_summers = TRUE), 5)
  expect_equal(count_seasons(20032, 20054, ignore_summers = TRUE), 5)
  expect_equal(count_seasons(20032, 20062, ignore_summers = TRUE), 6)
})

test_that("Season Difference - Start Fall - Crossing 2016 collection switch", {
  # These were calculated by hand, by Alex Axthelm,
  # by counting the seasons in between each term
  # Start in Fall:
  expect_equal(count_seasons(20152, 20152, ignore_summers = FALSE), 0)
  expect_equal(count_seasons(20152, 20153, ignore_summers = FALSE), 1)
  expect_equal(count_seasons(20152, 20154, ignore_summers = FALSE), 2)
  expect_equal(count_seasons(20152, 20161, ignore_summers = FALSE), 2)
  expect_equal(count_seasons(20152, 20162, ignore_summers = FALSE), 3)
  expect_equal(count_seasons(20152, 20163, ignore_summers = FALSE), 4)
  # Here is the switch to trailing summer
  expect_equal(count_seasons(20152, 20165, ignore_summers = FALSE), 5)
  expect_equal(count_seasons(20152, 20172, ignore_summers = FALSE), 6)
  expect_equal(count_seasons(20152, 20173, ignore_summers = FALSE), 7)
  expect_equal(count_seasons(20152, 20175, ignore_summers = FALSE), 8)
  expect_equal(count_seasons(20152, 20182, ignore_summers = FALSE), 9)
  expect_equal(count_seasons(20152, 20183, ignore_summers = FALSE), 10)
  expect_equal(count_seasons(20152, 20185, ignore_summers = FALSE), 11)
  expect_equal(count_seasons(20152, 20192, ignore_summers = FALSE), 12)
})

test_that("Season Difference - Start Fall - Crossing 2016 - No Summers", {
  # These were calculated by hand, by Alex Axthelm,
  # by counting the seasons in between each term
  # Start in Fall:
  expect_equal(count_seasons(20152, 20152, ignore_summers = TRUE), 0)
  expect_equal(count_seasons(20152, 20153, ignore_summers = TRUE), 1)
  expect_equal(count_seasons(20152, 20154, ignore_summers = TRUE), 1)
  expect_equal(count_seasons(20152, 20161, ignore_summers = TRUE), 1)
  expect_equal(count_seasons(20152, 20162, ignore_summers = TRUE), 2)
  expect_equal(count_seasons(20152, 20163, ignore_summers = TRUE), 3)
  # Here is the switch to trailing summer
  expect_equal(count_seasons(20152, 20165, ignore_summers = TRUE), 3)
  expect_equal(count_seasons(20152, 20172, ignore_summers = TRUE), 4)
  expect_equal(count_seasons(20152, 20173, ignore_summers = TRUE), 5)
  expect_equal(count_seasons(20152, 20175, ignore_summers = TRUE), 5)
  expect_equal(count_seasons(20152, 20182, ignore_summers = TRUE), 6)
  expect_equal(count_seasons(20152, 20183, ignore_summers = TRUE), 7)
  expect_equal(count_seasons(20152, 20185, ignore_summers = TRUE), 7)
  expect_equal(count_seasons(20152, 20192, ignore_summers = TRUE), 8)
})

test_that("Season Difference - Start Spring - Crossing 2016", {
  # These were calculated by hand, by Alex Axthelm,
  # by counting the seasons in between each term
  # Start in Fall:
  expect_equal(count_seasons(20153, 20153, ignore_summers = FALSE), 0)
  expect_equal(count_seasons(20153, 20154, ignore_summers = FALSE), 1)
  expect_equal(count_seasons(20153, 20161, ignore_summers = FALSE), 1)
  expect_equal(count_seasons(20153, 20162, ignore_summers = FALSE), 2)
  expect_equal(count_seasons(20153, 20163, ignore_summers = FALSE), 3)
  # Here is the switch to trailing summer
  expect_equal(count_seasons(20153, 20165, ignore_summers = FALSE), 4)
  expect_equal(count_seasons(20153, 20172, ignore_summers = FALSE), 5)
  expect_equal(count_seasons(20153, 20173, ignore_summers = FALSE), 6)
  expect_equal(count_seasons(20153, 20175, ignore_summers = FALSE), 7)
  expect_equal(count_seasons(20153, 20182, ignore_summers = FALSE), 8)
  expect_equal(count_seasons(20153, 20183, ignore_summers = FALSE), 9)
  expect_equal(count_seasons(20153, 20185, ignore_summers = FALSE), 10)
  expect_equal(count_seasons(20153, 20192, ignore_summers = FALSE), 11)
})

test_that("Season Difference - Start Spring - Crossing 2016 - No Summers", {
  # These were calculated by hand, by Alex Axthelm,
  # by counting the seasons in between each term
  # Start in Fall:
  expect_equal(count_seasons(20153, 20153, ignore_summers = TRUE), 0)
  expect_equal(count_seasons(20153, 20154, ignore_summers = TRUE), 0)
  expect_equal(count_seasons(20153, 20161, ignore_summers = TRUE), 0)
  expect_equal(count_seasons(20153, 20162, ignore_summers = TRUE), 1)
  expect_equal(count_seasons(20153, 20163, ignore_summers = TRUE), 2)
  # Here is the switch to trailing summer
  expect_equal(count_seasons(20153, 20165, ignore_summers = TRUE), 2)
  expect_equal(count_seasons(20153, 20172, ignore_summers = TRUE), 3)
  expect_equal(count_seasons(20153, 20173, ignore_summers = TRUE), 4)
  expect_equal(count_seasons(20153, 20175, ignore_summers = TRUE), 4)
  expect_equal(count_seasons(20153, 20182, ignore_summers = TRUE), 5)
  expect_equal(count_seasons(20153, 20183, ignore_summers = TRUE), 6)
  expect_equal(count_seasons(20153, 20185, ignore_summers = TRUE), 6)
  expect_equal(count_seasons(20153, 20192, ignore_summers = TRUE), 7)
})

test_that("Season Difference - Start Summer1 - Crossing 2016", {
  # These were calculated by hand, by Alex Axthelm,
  # by counting the seasons in between each term
  # Start in Fall:
  expect_equal(count_seasons(20154, 20154, ignore_summers = FALSE), 0)
  expect_equal(count_seasons(20154, 20161, ignore_summers = FALSE), 0)
  expect_equal(count_seasons(20154, 20162, ignore_summers = FALSE), 1)
  expect_equal(count_seasons(20154, 20163, ignore_summers = FALSE), 2)
  # Here is the switch to trailing summer
  expect_equal(count_seasons(20154, 20165, ignore_summers = FALSE), 3)
  expect_equal(count_seasons(20154, 20172, ignore_summers = FALSE), 4)
  expect_equal(count_seasons(20154, 20173, ignore_summers = FALSE), 5)
  expect_equal(count_seasons(20154, 20175, ignore_summers = FALSE), 6)
  expect_equal(count_seasons(20154, 20182, ignore_summers = FALSE), 7)
  expect_equal(count_seasons(20154, 20183, ignore_summers = FALSE), 8)
  expect_equal(count_seasons(20154, 20185, ignore_summers = FALSE), 9)
  expect_equal(count_seasons(20154, 20192, ignore_summers = FALSE), 10)
})

test_that("Season Difference - Start Summer1 - Crossing 2016 - No Summers", {
  # These were calculated by hand, by Alex Axthelm,
  # by counting the seasons in between each term
  # Start in Fall:
  expect_equal(count_seasons(20154, 20154, ignore_summers = TRUE), 0)
  expect_equal(count_seasons(20154, 20161, ignore_summers = TRUE), 0)
  expect_equal(count_seasons(20154, 20162, ignore_summers = TRUE), 1)
  expect_equal(count_seasons(20154, 20163, ignore_summers = TRUE), 2)
  # Here is the switch to trailing summer
  expect_equal(count_seasons(20154, 20165, ignore_summers = TRUE), 2)
  expect_equal(count_seasons(20154, 20172, ignore_summers = TRUE), 3)
  expect_equal(count_seasons(20154, 20173, ignore_summers = TRUE), 4)
  expect_equal(count_seasons(20154, 20175, ignore_summers = TRUE), 4)
  expect_equal(count_seasons(20154, 20182, ignore_summers = TRUE), 5)
  expect_equal(count_seasons(20154, 20183, ignore_summers = TRUE), 6)
  expect_equal(count_seasons(20154, 20185, ignore_summers = TRUE), 6)
  expect_equal(count_seasons(20154, 20192, ignore_summers = TRUE), 7)
})

test_that("Season Difference - Start Fall - after 2016", {
  # These were calculated by hand, by Alex Axthelm,
  # by counting the seasons in between each term
  # Start in Fall:
  expect_equal(count_seasons(20172, 20172, ignore_summers = FALSE), 0)
  expect_equal(count_seasons(20172, 20173, ignore_summers = FALSE), 1)
  expect_equal(count_seasons(20172, 20175, ignore_summers = FALSE), 2)
  expect_equal(count_seasons(20172, 20182, ignore_summers = FALSE), 3)
  expect_equal(count_seasons(20172, 20183, ignore_summers = FALSE), 4)
  expect_equal(count_seasons(20172, 20185, ignore_summers = FALSE), 5)
  expect_equal(count_seasons(20172, 20192, ignore_summers = FALSE), 6)
})

test_that("Season Difference - Start Fall - after 2016 - No Summers", {
  # These were calculated by hand, by Alex Axthelm,
  # by counting the seasons in between each term
  # Start in Fall:
  expect_equal(count_seasons(20172, 20172, ignore_summers = TRUE), 0)
  expect_equal(count_seasons(20172, 20173, ignore_summers = TRUE), 1)
  expect_equal(count_seasons(20172, 20175, ignore_summers = TRUE), 1)
  expect_equal(count_seasons(20172, 20182, ignore_summers = TRUE), 2)
  expect_equal(count_seasons(20172, 20183, ignore_summers = TRUE), 3)
  expect_equal(count_seasons(20172, 20185, ignore_summers = TRUE), 3)
  expect_equal(count_seasons(20172, 20192, ignore_summers = TRUE), 4)
})

test_that("Season Difference - Start trailSum - after 2016", {
  # These were calculated by hand, by Alex Axthelm,
  # by counting the seasons in between each term
  # Start in Fall:
  expect_equal(count_seasons(20175, 20175, ignore_summers = FALSE), 0)
  expect_equal(count_seasons(20175, 20182, ignore_summers = FALSE), 1)
  expect_equal(count_seasons(20175, 20183, ignore_summers = FALSE), 2)
  expect_equal(count_seasons(20175, 20185, ignore_summers = FALSE), 3)
  expect_equal(count_seasons(20175, 20192, ignore_summers = FALSE), 4)
})

test_that("Season Difference - Start TrailSum - after 2016 - No Summers", {
  # These were calculated by hand, by Alex Axthelm,
  # by counting the seasons in between each term
  # Start in Fall:
  expect_equal(count_seasons(20175, 20175, ignore_summers = TRUE), 0)
  expect_equal(count_seasons(20175, 20182, ignore_summers = TRUE), 1)
  expect_equal(count_seasons(20175, 20183, ignore_summers = TRUE), 2)
  expect_equal(count_seasons(20175, 20185, ignore_summers = TRUE), 2)
  expect_equal(count_seasons(20175, 20192, ignore_summers = TRUE), 3)
})

# nolint start # I know the chances I'm taking, but I want longer lines
# Let's test going backwards in time, with termkey 1 bigger
test_that("Going Backwards, Spring, neg", {
  # These were calculated by hand, by Alex Axthelm,
  # by counting the seasons in between each term
  # Start in Fall:
  expect_equal(count_seasons(20183, 20152, ignore_summers = FALSE, neg = TRUE), -10)
  expect_equal(count_seasons(20183, 20153, ignore_summers = FALSE, neg = TRUE), -9)
  expect_equal(count_seasons(20183, 20154, ignore_summers = FALSE, neg = TRUE), -8)
  expect_equal(count_seasons(20183, 20161, ignore_summers = FALSE, neg = TRUE), -8)
  expect_equal(count_seasons(20183, 20162, ignore_summers = FALSE, neg = TRUE), -7)
  expect_equal(count_seasons(20183, 20163, ignore_summers = FALSE, neg = TRUE), -6)
  expect_equal(count_seasons(20183, 20165, ignore_summers = FALSE, neg = TRUE), -5)
  expect_equal(count_seasons(20183, 20172, ignore_summers = FALSE, neg = TRUE), -4)
  expect_equal(count_seasons(20183, 20173, ignore_summers = FALSE, neg = TRUE), -3)
  expect_equal(count_seasons(20183, 20175, ignore_summers = FALSE, neg = TRUE), -2)
  expect_equal(count_seasons(20183, 20182, ignore_summers = FALSE, neg = TRUE), -1)
  expect_equal(count_seasons(20183, 20183, ignore_summers = FALSE, neg = TRUE), 0)
})

test_that("Going Backwards, Spring, neg, nosum", {
  # These were calculated by hand, by Alex Axthelm,
  # by counting the seasons in between each term
  # Start in Fall:
  expect_equal(count_seasons(20183, 20152, ignore_summers = TRUE, neg = TRUE), -7)
  expect_equal(count_seasons(20183, 20153, ignore_summers = TRUE, neg = TRUE), -6)
  expect_equal(count_seasons(20183, 20154, ignore_summers = TRUE, neg = TRUE), -5)
  expect_equal(count_seasons(20183, 20161, ignore_summers = TRUE, neg = TRUE), -5)
  expect_equal(count_seasons(20183, 20162, ignore_summers = TRUE, neg = TRUE), -5)
  expect_equal(count_seasons(20183, 20163, ignore_summers = TRUE, neg = TRUE), -4)
  expect_equal(count_seasons(20183, 20165, ignore_summers = TRUE, neg = TRUE), -3)
  expect_equal(count_seasons(20183, 20172, ignore_summers = TRUE, neg = TRUE), -3)
  expect_equal(count_seasons(20183, 20173, ignore_summers = TRUE, neg = TRUE), -2)
  expect_equal(count_seasons(20183, 20175, ignore_summers = TRUE, neg = TRUE), -1)
  expect_equal(count_seasons(20183, 20182, ignore_summers = TRUE, neg = TRUE), -1)
  expect_equal(count_seasons(20183, 20183, ignore_summers = TRUE, neg = TRUE), 0)
})

test_that("Going Backwards, Fall, neg", {
  # These were calculated by hand, by Alex Axthelm,
  # by counting the seasons in between each term
  # Start in Fall:
  expect_equal(count_seasons(20182, 20152, ignore_summers = FALSE, neg = TRUE), -9)
  expect_equal(count_seasons(20182, 20153, ignore_summers = FALSE, neg = TRUE), -8)
  expect_equal(count_seasons(20182, 20154, ignore_summers = FALSE, neg = TRUE), -7)
  expect_equal(count_seasons(20182, 20161, ignore_summers = FALSE, neg = TRUE), -7)
  expect_equal(count_seasons(20182, 20162, ignore_summers = FALSE, neg = TRUE), -6)
  expect_equal(count_seasons(20182, 20163, ignore_summers = FALSE, neg = TRUE), -5)
  expect_equal(count_seasons(20182, 20165, ignore_summers = FALSE, neg = TRUE), -4)
  expect_equal(count_seasons(20182, 20172, ignore_summers = FALSE, neg = TRUE), -3)
  expect_equal(count_seasons(20182, 20173, ignore_summers = FALSE, neg = TRUE), -2)
  expect_equal(count_seasons(20182, 20175, ignore_summers = FALSE, neg = TRUE), -1)
  expect_equal(count_seasons(20182, 20182, ignore_summers = FALSE, neg = TRUE), 0)
})

test_that("Going Backwards, Fall, neg, nosum", {
  # These were calculated by hand, by Alex Axthelm,
  # by counting the seasons in between each term
  # Start in Fall:
  expect_equal(count_seasons(20182, 20152, ignore_summers = TRUE, neg = TRUE), -6)
  expect_equal(count_seasons(20182, 20153, ignore_summers = TRUE, neg = TRUE), -5)
  expect_equal(count_seasons(20182, 20154, ignore_summers = TRUE, neg = TRUE), -4)
  expect_equal(count_seasons(20182, 20161, ignore_summers = TRUE, neg = TRUE), -4)
  expect_equal(count_seasons(20182, 20162, ignore_summers = TRUE, neg = TRUE), -4)
  expect_equal(count_seasons(20182, 20163, ignore_summers = TRUE, neg = TRUE), -3)
  expect_equal(count_seasons(20182, 20165, ignore_summers = TRUE, neg = TRUE), -2)
  expect_equal(count_seasons(20182, 20172, ignore_summers = TRUE, neg = TRUE), -2)
  expect_equal(count_seasons(20182, 20173, ignore_summers = TRUE, neg = TRUE), -1)
  expect_equal(count_seasons(20182, 20175, ignore_summers = TRUE, neg = TRUE), 0)
  expect_equal(count_seasons(20182, 20182, ignore_summers = TRUE, neg = TRUE), 0)
})

test_that("Going Backwards, Summer, neg", {
  # These were calculated by hand, by Alex Axthelm,
  # by counting the seasons in between each term
  # Start in Fall:
  expect_equal(count_seasons(20175, 20152, ignore_summers = FALSE, neg = TRUE), -8)
  expect_equal(count_seasons(20175, 20153, ignore_summers = FALSE, neg = TRUE), -7)
  expect_equal(count_seasons(20175, 20154, ignore_summers = FALSE, neg = TRUE), -6)
  expect_equal(count_seasons(20175, 20161, ignore_summers = FALSE, neg = TRUE), -6)
  expect_equal(count_seasons(20175, 20162, ignore_summers = FALSE, neg = TRUE), -5)
  expect_equal(count_seasons(20175, 20163, ignore_summers = FALSE, neg = TRUE), -4)
  expect_equal(count_seasons(20175, 20165, ignore_summers = FALSE, neg = TRUE), -3)
  expect_equal(count_seasons(20175, 20172, ignore_summers = FALSE, neg = TRUE), -2)
  expect_equal(count_seasons(20175, 20173, ignore_summers = FALSE, neg = TRUE), -1)
  expect_equal(count_seasons(20175, 20175, ignore_summers = FALSE, neg = TRUE), 0)
})

test_that("Going Backwards, Summer, neg, nosum", {
  # These were calculated by hand, by Alex Axthelm,
  # by counting the seasons in between each term
  # Start in Fall:
  expect_equal(count_seasons(20175, 20152, ignore_summers = TRUE, neg = TRUE), -6)
  expect_equal(count_seasons(20175, 20153, ignore_summers = TRUE, neg = TRUE), -5)
  expect_equal(count_seasons(20175, 20154, ignore_summers = TRUE, neg = TRUE), -4)
  expect_equal(count_seasons(20175, 20161, ignore_summers = TRUE, neg = TRUE), -4)
  expect_equal(count_seasons(20175, 20162, ignore_summers = TRUE, neg = TRUE), -4)
  expect_equal(count_seasons(20175, 20163, ignore_summers = TRUE, neg = TRUE), -3)
  expect_equal(count_seasons(20175, 20165, ignore_summers = TRUE, neg = TRUE), -2)
  expect_equal(count_seasons(20175, 20172, ignore_summers = TRUE, neg = TRUE), -2)
  expect_equal(count_seasons(20175, 20173, ignore_summers = TRUE, neg = TRUE), -1)
  expect_equal(count_seasons(20175, 20175, ignore_summers = TRUE, neg = TRUE), 0)
})

# ANDnow with neg = FALSE
test_that("Going Backwards, Spring, noneg", {
  # These were calculated by hand, by Alex Axthelm,
  # by counting the seasons in between each term
  # Start in Fall:
  expect_equal(count_seasons(20183, 20152, ignore_summers = FALSE, neg = FALSE), 10)
  expect_equal(count_seasons(20183, 20153, ignore_summers = FALSE, neg = FALSE), 9)
  expect_equal(count_seasons(20183, 20154, ignore_summers = FALSE, neg = FALSE), 8)
  expect_equal(count_seasons(20183, 20161, ignore_summers = FALSE, neg = FALSE), 8)
  expect_equal(count_seasons(20183, 20162, ignore_summers = FALSE, neg = FALSE), 7)
  expect_equal(count_seasons(20183, 20163, ignore_summers = FALSE, neg = FALSE), 6)
  expect_equal(count_seasons(20183, 20165, ignore_summers = FALSE, neg = FALSE), 5)
  expect_equal(count_seasons(20183, 20172, ignore_summers = FALSE, neg = FALSE), 4)
  expect_equal(count_seasons(20183, 20173, ignore_summers = FALSE, neg = FALSE), 3)
  expect_equal(count_seasons(20183, 20175, ignore_summers = FALSE, neg = FALSE), 2)
  expect_equal(count_seasons(20183, 20182, ignore_summers = FALSE, neg = FALSE), 1)
  expect_equal(count_seasons(20183, 20183, ignore_summers = FALSE, neg = FALSE), 0)
})

test_that("Going Backwards, Spring, neg, nosum", {
  # These were calculated by hand, by Alex Axthelm,
  # by counting the seasons in between each term
  # Start in Fall:
  expect_equal(count_seasons(20183, 20152, ignore_summers = TRUE, neg = FALSE), 7)
  expect_equal(count_seasons(20183, 20153, ignore_summers = TRUE, neg = FALSE), 6)
  expect_equal(count_seasons(20183, 20154, ignore_summers = TRUE, neg = FALSE), 5)
  expect_equal(count_seasons(20183, 20161, ignore_summers = TRUE, neg = FALSE), 5)
  expect_equal(count_seasons(20183, 20162, ignore_summers = TRUE, neg = FALSE), 5)
  expect_equal(count_seasons(20183, 20163, ignore_summers = TRUE, neg = FALSE), 4)
  expect_equal(count_seasons(20183, 20165, ignore_summers = TRUE, neg = FALSE), 3)
  expect_equal(count_seasons(20183, 20172, ignore_summers = TRUE, neg = FALSE), 3)
  expect_equal(count_seasons(20183, 20173, ignore_summers = TRUE, neg = FALSE), 2)
  expect_equal(count_seasons(20183, 20175, ignore_summers = TRUE, neg = FALSE), 1)
  expect_equal(count_seasons(20183, 20182, ignore_summers = TRUE, neg = FALSE), 1)
  expect_equal(count_seasons(20183, 20183, ignore_summers = TRUE, neg = FALSE), 0)
})

test_that("Going Backwards, Fall, noneg", {
  # These were calculated by hand, by Alex Axthelm,
  # by counting the seasons in between each term
  # Start in Fall:
  expect_equal(count_seasons(20182, 20152, ignore_summers = FALSE, neg = FALSE), 9)
  expect_equal(count_seasons(20182, 20153, ignore_summers = FALSE, neg = FALSE), 8)
  expect_equal(count_seasons(20182, 20154, ignore_summers = FALSE, neg = FALSE), 7)
  expect_equal(count_seasons(20182, 20161, ignore_summers = FALSE, neg = FALSE), 7)
  expect_equal(count_seasons(20182, 20162, ignore_summers = FALSE, neg = FALSE), 6)
  expect_equal(count_seasons(20182, 20163, ignore_summers = FALSE, neg = FALSE), 5)
  expect_equal(count_seasons(20182, 20165, ignore_summers = FALSE, neg = FALSE), 4)
  expect_equal(count_seasons(20182, 20172, ignore_summers = FALSE, neg = FALSE), 3)
  expect_equal(count_seasons(20182, 20173, ignore_summers = FALSE, neg = FALSE), 2)
  expect_equal(count_seasons(20182, 20175, ignore_summers = FALSE, neg = FALSE), 1)
  expect_equal(count_seasons(20182, 20182, ignore_summers = FALSE, neg = FALSE), 0)
})

test_that("Going Backwards, Fall, neg, nosum", {
  # These were calculated by hand, by Alex Axthelm,
  # by counting the seasons in between each term
  # Start in Fall:
  expect_equal(count_seasons(20182, 20152, ignore_summers = TRUE, neg = FALSE), 6)
  expect_equal(count_seasons(20182, 20153, ignore_summers = TRUE, neg = FALSE), 5)
  expect_equal(count_seasons(20182, 20154, ignore_summers = TRUE, neg = FALSE), 4)
  expect_equal(count_seasons(20182, 20161, ignore_summers = TRUE, neg = FALSE), 4)
  expect_equal(count_seasons(20182, 20162, ignore_summers = TRUE, neg = FALSE), 4)
  expect_equal(count_seasons(20182, 20163, ignore_summers = TRUE, neg = FALSE), 3)
  expect_equal(count_seasons(20182, 20165, ignore_summers = TRUE, neg = FALSE), 2)
  expect_equal(count_seasons(20182, 20172, ignore_summers = TRUE, neg = FALSE), 2)
  expect_equal(count_seasons(20182, 20173, ignore_summers = TRUE, neg = FALSE), 1)
  expect_equal(count_seasons(20182, 20175, ignore_summers = TRUE, neg = FALSE), 0)
  expect_equal(count_seasons(20182, 20182, ignore_summers = TRUE, neg = FALSE), 0)
})

test_that("Going Backwards, Summer, noneg", {
  # These were calculated by hand, by Alex Axthelm,
  # by counting the seasons in between each term
  # Start in Fall:
  expect_equal(count_seasons(20175, 20152, ignore_summers = FALSE, neg = FALSE), 8)
  expect_equal(count_seasons(20175, 20153, ignore_summers = FALSE, neg = FALSE), 7)
  expect_equal(count_seasons(20175, 20154, ignore_summers = FALSE, neg = FALSE), 6)
  expect_equal(count_seasons(20175, 20161, ignore_summers = FALSE, neg = FALSE), 6)
  expect_equal(count_seasons(20175, 20162, ignore_summers = FALSE, neg = FALSE), 5)
  expect_equal(count_seasons(20175, 20163, ignore_summers = FALSE, neg = FALSE), 4)
  expect_equal(count_seasons(20175, 20165, ignore_summers = FALSE, neg = FALSE), 3)
  expect_equal(count_seasons(20175, 20172, ignore_summers = FALSE, neg = FALSE), 2)
  expect_equal(count_seasons(20175, 20173, ignore_summers = FALSE, neg = FALSE), 1)
  expect_equal(count_seasons(20175, 20175, ignore_summers = FALSE, neg = FALSE), 0)
})

test_that("Going Backwards, Summer, neg, nosum", {
  # These were calculated by hand, by Alex Axthelm,
  # by counting the seasons in between each term
  # Start in Fall:
  expect_equal(count_seasons(20175, 20152, ignore_summers = TRUE, neg = FALSE), 6)
  expect_equal(count_seasons(20175, 20153, ignore_summers = TRUE, neg = FALSE), 5)
  expect_equal(count_seasons(20175, 20154, ignore_summers = TRUE, neg = FALSE), 4)
  expect_equal(count_seasons(20175, 20161, ignore_summers = TRUE, neg = FALSE), 4)
  expect_equal(count_seasons(20175, 20162, ignore_summers = TRUE, neg = FALSE), 4)
  expect_equal(count_seasons(20175, 20163, ignore_summers = TRUE, neg = FALSE), 3)
  expect_equal(count_seasons(20175, 20165, ignore_summers = TRUE, neg = FALSE), 2)
  expect_equal(count_seasons(20175, 20172, ignore_summers = TRUE, neg = FALSE), 2)
  expect_equal(count_seasons(20175, 20173, ignore_summers = TRUE, neg = FALSE), 1)
  expect_equal(count_seasons(20175, 20175, ignore_summers = TRUE, neg = FALSE), 0)
})

test_that("vectorized function works", {
  expect_equal(count_seasons(rep(20023, 13),
      c(
        20023,
        20024,
        20031,
        20032,
        20033,
        20034,
        20042,
        20043,
        20044,
        20052,
        20053,
        20054,
        20062
        ), ignore_summers = FALSE,
      neg = FALSE
      ),
    c(
      0,
      1,
      1,
      2,
      3,
      4,
      5,
      6,
      7,
      8,
      9,
      10,
      11
      ))
  expect_equal(count_seasons(rep(20024, 12),
      c(
        20024,
        20031,
        20032,
        20033,
        20034,
        20042,
        20043,
        20044,
        20052,
        20053,
        20054,
        20062
        ), ignore_summers = TRUE,
      neg = FALSE
      ),
    c(
        0,
        0,
        1,
        2,
        2,
        3,
        4,
        4,
        5,
        6,
        6,
        7
      ))
  expect_equal(count_seasons(rep(20183, 12),
      c(
          20152,
          20153,
          20154,
          20161,
          20162,
          20163,
          20165,
          20172,
          20173,
          20175,
          20182,
          20183
        ), ignore_summers = FALSE,
      neg = TRUE
      ),
    c(
        -10,
        -9,
        -8,
        -8,
        -7,
        -6,
        -5,
        -4,
        -3,
        -2,
        -1,
        0
      ))
  expect_equal(count_seasons(rep(20175, 10),
      c(
          20152,
          20153,
          20154,
          20161,
          20162,
          20163,
          20165,
          20172,
          20173,
          20175
        ), ignore_summers = TRUE,
      neg = FALSE
      ),
    c(
        6,
        5,
        4,
        4,
        4,
        3,
        2,
        2,
        1,
        0
      ))
})

     # nolint end  # Back to safety
