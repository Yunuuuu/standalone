band_members <- data.frame(
  name = c("Mick", "John", "Paul"),
  band = c("Stones", "Beatles", "Beatles")
)

band_instruments <- data.frame(
  name = c("John", "Paul", "Keith"),
  plays = c("guitar", "bass", "guitar")
)

test_that("join works well", {
  expect_snapshot_value(
    full_join(band_members, band_instruments),
    style = "json2"
  )
  expect_snapshot_value(
    inner_join(band_members, band_instruments),
    style = "json2"
  )
  expect_snapshot_value(
    left_join(band_members, band_instruments),
    style = "json2"
  )
  expect_snapshot_value(
    right_join(band_members, band_instruments),
    style = "json2"
  )
  expect_snapshot_value(
    cross_join(band_members, band_instruments),
    style = "json2"
  )
})

test_that("case_when() works well", {
  x <- 1:70
  testthat::expect_identical(
    dplyr::case_when(
      x %% 35 == 0 ~ "fizz buzz",
      x %% 5 == 0 ~ "fizz",
      x %% 7 == 0 ~ "buzz",
      .default = as.character(x)
    ),
    case_when(
      as.character(1:70),
      x %% 35 == 0 ~ "fizz buzz",
      x %% 5 == 0 ~ "fizz",
      x %% 7 == 0 ~ "buzz"
    )
  )
  testthat::expect_identical(
    dplyr::case_when(
      x %% 5 == 0 ~ "fizz",
      x %% 7 == 0 ~ "buzz",
      x %% 35 == 0 ~ "fizz buzz",
      .default = as.character(x)
    ),
    case_when(
      as.character(1:70),
      x %% 5 == 0 ~ "fizz",
      x %% 7 == 0 ~ "buzz",
      x %% 35 == 0 ~ "fizz buzz"
    )
  )
})
