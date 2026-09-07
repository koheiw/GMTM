
test_that("get_threads works", {

  options(GMTM.threads = 3)
  expect_equal(
    GMTM:::get_threads(), 3
  )

  options(GMTM.threads = "xxx")
  expect_error(
    GMTM:::get_threads(),
    "GMTM.threads must be an integer",
    fixed = TRUE
  )

  # restore
  options(GMTM.threads = 2)

})

test_that("OMP is enabled", {

  skip_on_os("mac")

  expect_true(
    GMTM:::cpp_omp_enabled()
  )

})
