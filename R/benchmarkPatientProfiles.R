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

#' Benchmark the PatientPrfiles packages for a certain cdm object
#'
#' @param cdm A cdm_reference.
#' @param prevalence Fraction of population included in the study table.
#' @param seed Random number seed.
#'
#' @export
#'
#' @examples
#' \donttest{
#' library(PatientProfiles)
#'
#' cdm <- mockPatientProfiles()
#'
#' benchmarkPatientProfiles(cdm)
#' }
#'
benchmarkPatientProfiles <- function(cdm, prevalence = 0.8, seed = 123456) {
  # initial checks
  checkCdm(cdm)

  # list of parameters
  parameters <- list(
    "prevalence" = prevalence, "seed" = seed
  )

  # create result object
  result <- createBenchmarkResult(cdm, parameters)

  # create target cohort
  result <- createStudyCohort(result, cdm, prevalence, seed)

  # create total time
  result <- totalTime(result)

  return(result)
}

createStudyCohort <- function(result, cdm, prevalence, seed) {
  tictoc::tic()
  cdm$cohort1 <- createCohort(cdm = cdm, prevalence = prevalence, seed = seed)
  time <- tictoc::toc(quiet = TRUE)
  time <- as.numeric(time$toc - time$tic)
  result <- appendNewTime(result, time, "create study cohort")
  return(result)
}
totalTime <- function(result) {
  cols <- colnames(result)
  cols <- cols[!(cols %in% c(
    "task", "time_taken_secs", "time_taken_mins", "time_taken_hours"
  ))]
  result <- result %>%
    dplyr::union_all(
      result %>%
        dplyr::group_by(dplyr::pick(dplyr::all_of(cols))) %>%
        dplyr::summarise(
          time_taken_secs = sum(.data$time_taken_secs),
          time_taken_mins = sum(.data$time_taken_mins),
          time_taken_hours = sum(.data$time_taken_hours)
        ) %>%
        dplyr::mutate(task = "TOTAL TIME")
    )
  return(result)
}
