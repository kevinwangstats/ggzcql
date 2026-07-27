test_that("pal_zcql returns correct colors", {
  pal <- pal_zcql()
  cols <- pal(5)
  expect_equal(length(cols), 5)
  expect_equal(cols[1], "#1E88E5")
  expect_equal(cols[2], "#388E3C")
  expect_equal(cols[3], "#D32F2F")
  expect_equal(cols[4], "#7CB342")
  expect_equal(cols[5], "#8D6E63")
})

test_that("pal_zcql supports alpha modification", {
  pal_alpha <- pal_zcql(alpha = 0.5)
  cols <- pal_alpha(2)
  expect_equal(nchar(cols[1]), 9) # #RRGGBBAA format
})

test_that("scale_color_zcql and scale_fill_zcql return scale objects", {
  sc_color <- scale_color_zcql()
  sc_fill <- scale_fill_zcql()
  expect_s3_class(sc_color, "ScaleDiscrete")
  expect_s3_class(sc_fill, "ScaleDiscrete")
})

test_that("theme_zcql sets the correct background color", {
  th <- theme_zcql()
  expect_s3_class(th, "theme")
  expect_equal(th$plot.background$fill, "#E8D7B0")
  expect_equal(th$panel.background$fill, "#E8D7B0")
})
