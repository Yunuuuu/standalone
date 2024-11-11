band_members <- data.frame(
  name = c("Mick", "John", "Paul"),
  band = c("Stones", "Beatles", "Beatles")
)

band_instruments <- data.frame(
  name = c("John", "Paul", "Keith"),
  plays = c("guitar", "bass", "guitar")
)

test_that("vctrs works well", {
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
