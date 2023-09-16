# Copyright 2023 DARWIN EU (C)
#
# This file is part of CDMBenchmark
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

createBenchmarkResult <- function(cdm, parameters) {
  tictoc::tic()
  snap <- dplyr::tibble(snapshot = 1) #CDMConnector::snapshot(cdm)
  time <- tictoc::toc(quiet = TRUE)
  time <- as.numeric(time$toc - time$tic)
  task <- "create cdm snapshot"
  result <- dplyr::tibble(
    task = task, time_taken_secs = time, time_taken_mins = time/60,
    time_taken_hours = time/3600
  ) %>%
    dplyr::mutate(parameters = lapply(names(parameters), function(x) {
      paste0(x, ": ", parameters[[x]])
    }) %>% paste0(collapse = "; ")) %>%
    dplyr::bind_cols(snap)
  return(result)
}
appendNewTime <- function(benchmarkResult, time, task) {
  benchmarkResult %>%
    dplyr::union_all(
      benchmarkResult %>%
        dplyr::select(-c(
          "task", "time_taken_secs", "time_taken_mins", "time_taken_hours"
        )) %>%
        dplyr::distinct() %>%
        dplyr::mutate(
          "task" = .env$task, "time_taken_secs" = .env$time,
          "time_taken_mins" = .env$time/60, "time_taken_hours" = .env$time/3600
        )
    )
}
totalTime <- function(result) {
  tasks <- result$task
  tasks <- tasks[!(grepl("TOTAL", tasks))]
  result <- groupTotal(result, "TOTAL TIME", tasks)
  return(result)
}
groupTotal <- function(result, task, tasks) {
  cols <- colnames(result)
  cols <- cols[!(cols %in% c(
    "task", "time_taken_secs", "time_taken_mins", "time_taken_hours"
  ))]
  result <- result %>%
    dplyr::union_all(
      result %>%
        dplyr::group_by(dplyr::pick(dplyr::all_of(cols))) %>%
        dplyr::filter(.data$task %in% .env$tasks) %>%
        dplyr::summarise(
          time_taken_secs = sum(.data$time_taken_secs),
          time_taken_mins = sum(.data$time_taken_mins),
          time_taken_hours = sum(.data$time_taken_hours)
        ) %>%
        dplyr::mutate(task = .env$task)
    )
  return(result)
}
