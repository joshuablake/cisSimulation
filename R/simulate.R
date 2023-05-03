#' Simulate the CIS study
#'
#' @param positive_duration A duration function specifying the duration distribution to use
#' @param n_indivs The number of individuals (episodes) to simulate
#' @param testing_times A testing function specifying the times at which individuals are tested
#' @param infection_times An infections function specifying the shape of the epidemic
#' @param testing A sensitivity or other function specifying how to simulate the testing results
#' @import dplyr
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @export
simulate_cis = function(
    positive_duration,
    n_indivs = 1e6,
    testing_times = testing_fixed(),
    infection_times = infections_flat(),
    testing = sensitivity_fixed()
    ) {

  tbl_individuals = tibble(
    i = 1:n_indivs
  ) |>
    mutate(
      infection_time = infection_times(n()),
      duration = positive_duration(n())
    )

  testing_times(n_indivs) |>
    left_join(tbl_individuals, by = "i") |>
    mutate(test_result = testing(.data[["test_time"]] - .data[["infection_time"]], .data[["duration"]]))
}

#' Fixed interval testing times
#'
#' @param test_interval Time between tests.
#' @param first_test First possible testing time. Each individual will be tested first at a time between `first_test` and `first_test + test_interval - 1` chosen uniformly at random (this gives an even spread of testing times).
#' @param last_test Last possible testing time.
#' @import dplyr
#' @import tidyr
#' @importFrom tibble tibble
#' @importFrom rlang .data
#' @export
testing_fixed = function(test_interval = 7, first_test = -90, last_test = 300) {
  tests_after_first = seq.int(from = 0, to = last_test - first_test, by = test_interval)
  function(n_indivs) {
    tibble(
      i = 1:n_indivs
    ) |>
      mutate(
        tests_after_first = list(tests_after_first),
        i_first_test = sample.int(test_interval, n(), TRUE) - 1 + first_test,
      ) |>
      unnest("tests_after_first") |>
      transmute(.data[["i"]], test_time = .data[["i_first_test"]] + .data[["tests_after_first"]]) |>
      filter(between(.data[["test_time"]], first_test, last_test))
  }
}

#' Custom testing times
#'
#' @param tbl_times Tibble with two columns i (individual) and test_time for each test
#' @export
testing_custom = function(tbl_times) {
  schedule_ids = unique(tbl_times$i)
  stopifnot(all(is.finite(tbl_times$test_time)))
  function(n_indivs) {
    tibble::tibble(
      i = 1:n_indivs
    ) |>
      dplyr::mutate(
        test_schedule = sample(schedule_ids, n(), replace = TRUE)
      ) |>
      dplyr::left_join(tbl_times, by = c("test_schedule" = "i"))
  }
}

#' Specify a custom distribution for the duration of positivity
#'
#' @param pmf Probability mass function (vector) for the distribution (automatically normalised), assumed to have support 1:length(pmf)
#' @export
duration_custom = function(pmf) {
  function(n) {
    sample.int(length(pmf), n, replace = TRUE, prob = pmf)
  }
}

#' A flat epidemic
#' @export
infections_flat = function(possible_times = 1:100) {
  function(n) sample(possible_times, n, TRUE)
}

#' A fixed test sensitivity with no false positives
#'
#' @param sensitivity The test sensitivity to use, the default is no false negatives
#' @importFrom stats runif
#' @export
sensitivity_fixed = function(sensitivity = 1) {
  function(time_since_infection, duration) {
    stopifnot(length(time_since_infection) == length(duration))
    n = length(time_since_infection)
    (time_since_infection >= 0) & (time_since_infection < duration) & (runif(n) < sensitivity)
  }
}

#' A test sensitivity that decreases linearly over time to a constant value
#'
#' @param min,max Minimum and maximum test sensitivity to use
#' @param time_of_min Days after infection when minimum occurs
#' @importFrom stats approx
#' @importFrom stats runif
#' @export
sensitivity_linear = function(max = 0.9, min = 0.5, time_of_min = 50) {
  stopifnot(max <= 1)
  stopifnot(min >= 0)
  stopifnot(max >= min)
  stopifnot(time_of_min >= 1)
  function(time_since_infection, duration) {
    stopifnot(length(time_since_infection) == length(duration))
    n = length(time_since_infection)
    sensitivity = approx(
      x = c(0, time_of_min),
      y = c(max, min),
      xout = time_since_infection,
      rule = 2
    )$y
    (time_since_infection >= 0) & (time_since_infection < duration) & (runif(n) < sensitivity)
  }
}
