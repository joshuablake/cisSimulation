test_that("daily tests", {
  expected_len = 15
  result = testing_fixed(1, 0, 14)(1)
  expect_length(result$individual, expected_len)
  expect_equal(result$individual, rep(1, expected_len))
  expect_equal(result$test_time, 0:14)
})

test_that("weekly tests", {
  result = testing_fixed(7, 0, 20)(10)
  expect_true(all(table(result$individual) == 3))
  expect_equal(dplyr::n_distinct(result$individual), 10)
  expect_true(all(result$test_time >= 0))
  expect_true(all(result$test_time <= 20))
})

test_that("tests are uniform", {
  n_indiv = 100e3
  expected_len = 10 * n_indiv
  result = testing_fixed(10, 1, 100)(n_indiv)
  expect_true(all(table(result$individual) == 10))
  expect_equal(dplyr::n_distinct(result$individual), n_indiv)
  expect_true(all(result$test_time >= 1))
  expect_true(all(result$test_time <= 100))
  tests_by_day = table(result$test_time)
  expect_true(all(tests_by_day >= 9000))
  expect_true(all(tests_by_day <= 11000))
})

test_that("test results duration", {
  duration = 10
  times = -15:30
  input_len = length(times)
  result = sensitivity_fixed(1)(times, rep(duration, input_len))
  expect_true(all(result %in% c(0, 1)))
  expect_equal(sum(result), duration)
  expect_length(result, input_len)
})

test_that("test results sensitivity", {
  sensitivity = 0.5
  duration = 10
  times = rep(0, 1e6)
  input_len = length(times)
  result = sensitivity_fixed(sensitivity)(times, rep(duration, input_len))
  expect_true(all(result %in% c(0, 1)))
  expect_length(result, input_len)
  expect_lt(sum(result), 502e3)
  expect_gt(sum(result), 498e3)
})

test_that("test results sensitivity and duration", {
  sensitivity = 0.5
  duration = 10
  times = rep(-15:30, 1e5)
  input_len = length(times)
  result = sensitivity_fixed(sensitivity)(times, rep(duration, input_len))
  expect_true(all(result %in% c(0, 1)))
  expect_length(result, input_len)
  expect_lt(sum(result), 502e3)
  expect_gt(sum(result), 498e3)
})

test_that("test results sensitivity linear", {
  n_indiv = 1e6
  sensitivity = 0.5
  duration = 7
  uniq_times = 0:9
  true_pos_proportion = c(1, 0.9, 0.8, 0.7, 0.6, 0.5, 0.5, rep(0, 3))
  stopifnot(length(true_pos_proportion) == length(uniq_times))
  times = rep(uniq_times, n_indiv)
  input_len = length(times)
  result = sensitivity_linear(1, 0.5, 5)(times, rep(duration, input_len))
  expect_true(all(result %in% c(0, 1)))
  expect_length(result, input_len)

  result_summary = tibble::tibble(
    individual = rep(1:n_indiv, length(uniq_times)),
    time = times,
    result = result,
  ) |>
    dplyr::group_by(time) |>
    dplyr::summarise(n = n(), pos = sum(result), .groups = "drop") |>
    dplyr::arrange(time)
  expect_equal(result_summary$time, uniq_times)
  expect_true(all(result_summary$n == n_indiv))
  expect_true(all(result_summary$pos >= qbinom(1e-4, n_indiv, true_pos_proportion)))
  expect_true(all(result_summary$pos <= qbinom(1 - 1e-4, n_indiv, true_pos_proportion)))
})

test_that("test flat infections", {
  n = 10e3
  max = 10
  result = infections_flat(1:max)(n)
  expect_length(result, n)
  l999 = qbinom(0.001, n, 1/max)
  u999 = qbinom(0.999, n, 1/max)
  counts = table(result)
  expect_true(all(l999 <= counts))
  expect_true(all(u999 >= counts))
})
