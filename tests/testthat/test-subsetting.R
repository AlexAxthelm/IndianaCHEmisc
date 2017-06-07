context("subsetting")
# I'm going to use filter() a little later on
requireNamespace("dplyr")

chick_df <- as.data.frame(datasets::ChickWeight)
chick_df$Chick <- as.numeric(levels(chick_df$Chick)[chick_df$Chick])

## TODO: Rename context
## TODO: Add more tests

test_that("make_tiny_frame makes proper sized frames", {
  expect_equal(
    dim(make_tiny_frame(chick_df)),
    c(5, 4)
    )
  expect_equal(
    dim(make_tiny_frame(chick_df, sample_n = 37)),
    c(37, 4)
    )
  expect_equal(
    dim(make_tiny_frame(mtcars)),
    c(5, 11)
    )
  expect_equal(
    dim(make_tiny_frame(mtcars, sample_n = 37)),
    c(32, 11)
    # mtcars only has 32 entries
    )
  expect_equal(
    dim(make_tiny_frame(
          npk,
          keycolumn = "block"
        )),
    # Each block has 4 treatment combos
    c(20, 5)
    )
  expect_equal(
    dim(make_tiny_frame(
          npk,
          keycolumn = "block",
          sample_n = 2
        )),
    # Each block has 4 treatment combos
    c(8, 5)
    )
  expect_equal(
    dim(make_tiny_frame(
          npk,
          keycolumn = "block",
          sample_n = 17
        )),
    # There are only 24 samples
    c(24, 5)
    )
  expect_equal(
    dim(make_tiny_frame(
          npk,
          sample_n = 17
        )),
    # no keycolumn, selects from all
    c(17, 5)
    )
  expect_equal(
    dim(make_tiny_frame(
          npk,
          keycolumn = "K",
          sample_n = 1
        )),
    # The K factor is split evenly
    c(12, 5)
    )
  expect_equal(
    dim(make_tiny_frame(
          npk,
          sample_n = 0
        )),
    # This should return a frame with no rows
    c(0, 5)
    )
})

test_that("make_tiny_frame makes proper data in frames", {
  select_vec <- 1:13
  expect_true(all(
    make_tiny_frame(
      chick_df,
      keycolumn = "Chick",
      randomvec = select_vec
      ) ==
    dplyr::filter(
      chick_df,
      Chick <= 13
      )
    ))
  expect_true(all(
    class(make_tiny_frame(
      chick_df,
      keycolumn = "Chick",
      randomvec = select_vec
      )) ==
    class(dplyr::filter(
      chick_df,
      Chick <= 13
      ))
    ))

  select_vec <- sample(1:50, 24)
  expect_true(all(
    make_tiny_frame(
      chick_df,
      keycolumn = "Chick",
      randomvec = select_vec
      ) ==
    dplyr::filter(
      chick_df,
      Chick %in% select_vec
      )
    ))
  expect_true(all(
    class(make_tiny_frame(
      chick_df,
      keycolumn = "Chick",
      randomvec = select_vec
      )) ==
    class(dplyr::filter(
      chick_df,
      Chick <= 13
      ))
    ))

  select_vec <- chick_df$weight[chick_df$weight < 70]
  expect_true(all(
    make_tiny_frame(
      chick_df,
      keycolumn = "weight",
      randomvec = select_vec
      ) ==
    dplyr::filter(
      chick_df,
      weight < 70
      )
    ))
  expect_true(all(
    class(make_tiny_frame(
      chick_df,
      keycolumn = "weight",
      randomvec = select_vec
      )) ==
    class(dplyr::filter(
      chick_df,
      weight < 70
      ))
    ))
})
