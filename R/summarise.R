#' Summarise a simulation as episodes
#'
#' @param tbl_sims A tibble of tests as returned by positive_duration
#' @import dplyr
#' @importFrom rlang .data
#' @export
simulation_to_episodes = function(tbl_sims) {
  tbl_sims |>
    group_by(individual, infection_time, duration) |>
    summarise(
      n_pos = sum(.data[["test_result"]]),
      first_pos = min(c(.data[["test_time"]][test_result], Inf)),
      last_pos = max(c(.data[["test_time"]][test_result], -Inf)),
      prev_neg = max(c(.data[["test_time"]][.data[["test_time"]] < first_pos], -Inf)),
      next_neg = min(c(.data[["test_time"]][.data[["test_time"]] > last_pos], Inf)),
      second_neg = min(c(.data[["test_time"]][.data[["test_time"]] > next_neg], Inf)),
      n = sum(prev_neg <= .data[["test_time"]] & .data[["test_time"]] <= next_neg),
      test_times = list(.data[["test_time"]]),
      .groups = "drop"
    ) %>%
    mutate(type = case_when(
      n_pos == 0 ~ "missed infection",
      !is.finite(prev_neg) ~ "left-censored",
      !is.finite(next_neg) ~ "right-censored",
      TRUE ~ "captured",
    ))
}