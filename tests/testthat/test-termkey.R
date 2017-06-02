context("term key")

test_that("Correctly Split TermKeys", {

  na_list <- list(
    academic_year = NA_integer_,
    term_season = NA_character_,
    term_season_number = NA_integer_,
    termcode = NA_integer_,
    term_name = NA_character_,
    fiscal_year = NA_integer_,
    seasonkey = NA_integer_
  )

  expect_equal(split_termkey(-2), na_list)
  expect_equal(split_termkey(-1), na_list)
   expect_equal(split_termkey(0), na_list)
   expect_equal(split_termkey(1), na_list)
   expect_equal(split_termkey(2012), na_list)
   expect_equal(split_termkey(201214), na_list)
   expect_equal(
    split_termkey(20122),
    list(
      academic_year = 2012,
      term_season = "Fall",
      term_season_number = 2,
      termcode = 2,
      term_name = "Fall",
      fiscal_year = 2012,
      seasonkey = 20122
      )
    )
    expect_equal(
     split_termkey(20001),
     list(
       academic_year = 1999,
       term_season = "Summer",
       term_season_number = 5,
       termcode = 1,
       term_name = "Summer A",
       fiscal_year = 2000,
       seasonkey = 19995
       )
     )
    expect_equal(
     split_termkey(20183),
     list(
       academic_year = 2018,
       term_season = "Spring",
       term_season_number = 3,
       termcode = 3,
       term_name = "Spring",
       fiscal_year = 2018,
       seasonkey = 20183
       )
     )
    expect_equal(
     split_termkey(20154),
     list(
       academic_year = 2015,
       term_season = "Summer",
       term_season_number = 5,
       termcode = 4,
       term_name = "Summer B",
       fiscal_year = 2015,
       seasonkey = 20155
       )
     )
    expect_equal(
     split_termkey(20161),
     list(
       academic_year = 2015,
       term_season = "Summer",
       term_season_number = 5,
       termcode = 1,
       term_name = "Summer A",
       fiscal_year = NA_integer_,
       seasonkey = 20155
       )
     )
    expect_equal(
     split_termkey(20162),
     list(
       academic_year = 2016,
       term_season = "Fall",
       term_season_number = 2,
       termcode = 2,
       term_name = "Fall",
       fiscal_year = 2016,
       seasonkey = 20162
       )
     )
    expect_equal(
     split_termkey(20163),
     list(
       academic_year = 2016,
       term_season = "Spring",
       term_season_number = 3,
       termcode = 3,
       term_name = "Spring",
       fiscal_year = 2016,
       seasonkey = 20163
       )
     )
    expect_equal(split_termkey(20164), na_list)
    expect_equal(
     split_termkey(20165),
     list(
       academic_year = 2016,
       term_season = "Summer",
       term_season_number = 5,
       termcode = 5,
       term_name = "Trailing Summer",
       fiscal_year = 2016,
       seasonkey = 20165
       )
     )
  })
