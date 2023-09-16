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
benchmarkPatientProfiles <- function(cdm, studyCohort) {
  # initial checks
  #checkCdm(cdm = cdm, studyCohort = studyCohort)

  # list of parameters
  parameters <- list(
    studyCohort = studyCohort
  )

  # create result object
  #result <- createBenchmarkResult(cdm, parameters)
  result <- dplyr::tibble(
    task = "1", time_taken_secs = 1,
    time_taken_mins = 1, time_taken_hours = 1,
    parameters = "1", snapshot = "1"
  )

  # add demographics
  result <- benchmarkDemographics(result, cdm, studyCohort)

  # add cohort intersect
  result <- benchmarkCohortIntersect(result, cdm, studyCohort)

  # summarise result
  #result <- benchmarkSummariseResult(result, cdm, studyCohort)

  # summarise characteristics
  result <- benchmarkSummariseCharacteristics(result, cdm, studyCohort)

  # summarise large scale characteristics
  #result <- benchmarkSummariseLargeScaleCharacteristics(result, cdm, studyCohort)

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
benchmarkDemographics <- function(result, cdm, studyCohort) {
  tasks <- c(
    "add sex", "add age", "add 20-year bands age group",
    "add 5-year bands age group", "add future observation",
    "add prior observation", "add demographics - no age group",
    "add demographics - age groups"
  )

  # sex
  tictoc::tic()
  cdm[[studyCohort]] %>%
    PatientProfiles::addSex() %>%
    invisible()
  time <- tictoc::toc(quiet = TRUE)
  time <- as.numeric(time$toc - time$tic)
  result <- appendNewTime(result, time, tasks[1])

  # age
  tictoc::tic()
  cdm[[studyCohort]] %>%
    PatientProfiles::addAge() %>%
    invisible()
  time <- tictoc::toc(quiet = TRUE)
  time <- as.numeric(time$toc - time$tic)
  result <- appendNewTime(result, time, tasks[2])

  # age group (20 years)
  ageGroup1 <- list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150))
  tictoc::tic()
  cdm[[studyCohort]] %>%
    PatientProfiles::addAge(ageGroup = ageGroup1) %>%
    invisible()
  time <- tictoc::toc(quiet = TRUE)
  time <- as.numeric(time$toc - time$tic)
  result <- appendNewTime(result, time, tasks[3])

  # age group (5 years)
  ageGroup2 <- list(
    c(0, 4), c(5, 9), c(10, 14), c(15, 19), c(20, 24), c(25, 29), c(30, 34),
    c(35, 39), c(40, 44), c(45, 49), c(50, 54), c(55, 59), c(60, 64), c(65, 69),
    c(70, 74), c(75, 79), c(80, 84), c(85, 89), c(90, 94), c(95, 99),
    c(100, 150)
  )
  tictoc::tic()
  cdm[[studyCohort]] %>%
    PatientProfiles::addAge(ageGroup = ageGroup2) %>%
    invisible()
  time <- tictoc::toc(quiet = TRUE)
  time <- as.numeric(time$toc - time$tic)
  result <- appendNewTime(result, time, tasks[4])

  # future observation
  tictoc::tic()
  cdm[[studyCohort]] %>%
    PatientProfiles::addFutureObservation() %>%
    invisible()
  time <- tictoc::toc(quiet = TRUE)
  time <- as.numeric(time$toc - time$tic)
  result <- appendNewTime(result, time, tasks[5])

  # prior observation
  tictoc::tic()
  cdm[[studyCohort]] %>%
    PatientProfiles::addPriorObservation() %>%
    invisible()
  time <- tictoc::toc(quiet = TRUE)
  time <- as.numeric(time$toc - time$tic)
  result <- appendNewTime(result, time, tasks[6])

  # demographics all at once
  tictoc::tic()
  cdm[[studyCohort]] %>%
    PatientProfiles::addDemographics() %>%
    invisible()
  time <- tictoc::toc(quiet = TRUE)
  time <- as.numeric(time$toc - time$tic)
  result <- appendNewTime(result, time, tasks[7])

  # demographics all at once
  tictoc::tic()
  cdm[[studyCohort]] %>%
    PatientProfiles::addDemographics(ageGroup = list(ageGroup1, ageGroup2)) %>%
    invisible()
  time <- tictoc::toc(quiet = TRUE)
  time <- as.numeric(time$toc - time$tic)
  result <- appendNewTime(result, time, tasks[8])

  # summary
  result <- groupTotal(result, "TOTAL add demographics", tasks)

  return(result)
}
benchmarkCohortIntersect <- function(result, cdm, studyCohort) {
  tasks <- c(
    "add cohort intersect days", "add cohort intersect date",
    "add cohort intersect count", "add cohort intersect flag",
    "add cohort intersect all 1 window", "add cohort intersect all 5 windows"
  )

  # days
  tictoc::tic()
  cdm[[studyCohort]] %>%
    PatientProfiles::addCohortIntersectDays(targetCohortTable = studyCohort) %>%
    invisible()
  time <- tictoc::toc(quiet = TRUE)
  time <- as.numeric(time$toc - time$tic)
  result <- appendNewTime(result, time, tasks[1])

  # date
  tictoc::tic()
  cdm[[studyCohort]] %>%
    PatientProfiles::addCohortIntersectDate(targetCohortTable = studyCohort) %>%
    invisible()
  time <- tictoc::toc(quiet = TRUE)
  time <- as.numeric(time$toc - time$tic)
  result <- appendNewTime(result, time, tasks[2])

  # count
  tictoc::tic()
  cdm[[studyCohort]] %>%
    PatientProfiles::addCohortIntersectCount(targetCohortTable = studyCohort) %>%
    invisible()
  time <- tictoc::toc(quiet = TRUE)
  time <- as.numeric(time$toc - time$tic)
  result <- appendNewTime(result, time, tasks[3])

  # flag
  tictoc::tic()
  cdm[[studyCohort]] %>%
    PatientProfiles::addCohortIntersectFlag(targetCohortTable = studyCohort) %>%
    invisible()
  time <- tictoc::toc(quiet = TRUE)
  time <- as.numeric(time$toc - time$tic)
  result <- appendNewTime(result, time, tasks[4])

  # 1  window
  tictoc::tic()
  cdm[[studyCohort]] %>%
    PatientProfiles::addCohortIntersect(targetCohortTable = studyCohort) %>%
    invisible()
  time <- tictoc::toc(quiet = TRUE)
  time <- as.numeric(time$toc - time$tic)
  result <- appendNewTime(result, time, tasks[5])

  # 5  windows
  tictoc::tic()
  cdm[[studyCohort]] %>%
    PatientProfiles::addCohortIntersect(
      targetCohortTable = studyCohort,
      window = list(c(-Inf, -366), c(-365, -1), c(0, 0), c(1, 365), c(366, Inf))
    ) %>%
    invisible()
  time <- tictoc::toc(quiet = TRUE)
  time <- as.numeric(time$toc - time$tic)
  result <- appendNewTime(result, time, tasks[6])

  # summary
  result <- groupTotal(result, "TOTAL add cohort intersect", tasks)

  return(result)
}
benchmarkSummariseResult <- function(result, cdm, studyCohort) {
  # only binary, only numeric, only dates, only categorical
  return(result)
}
benchmarkSummariseCharacteristics <- function(result, cdm, studyCohort) {
  tasks <- c(
    "summarise characteristics default", "summarise characteristics age groups",
    "summarise characteristics number visits",
    "summarise characteristics covariate", "summarise characteristics all"
  )

  # default
  tictoc::tic()
  cdm[[studyCohort]] %>%
    PatientProfiles::summariseCharacteristics() %>%
    invisible()
  time <- tictoc::toc(quiet = TRUE)
  time <- as.numeric(time$toc - time$tic)
  result <- appendNewTime(result, time, tasks[1])

  # age group
  tictoc::tic()
  cdm[[studyCohort]] %>%
    PatientProfiles::summariseCharacteristics(ageGroup = list(
      c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150)
    ), tableIntersect = list()) %>%
    invisible()
  time <- tictoc::toc(quiet = TRUE)
  time <- as.numeric(time$toc - time$tic)
  result <- appendNewTime(result, time, tasks[2])

  # number visits
  tictoc::tic()
  cdm[[studyCohort]] %>%
    PatientProfiles::summariseCharacteristics(tableIntersect = list(
      "Visits in previous year" = list(
        tableName = "visit_occurrence", window = c(-365, 0), value = "count"
      )
    )) %>%
    invisible()
  time <- tictoc::toc(quiet = TRUE)
  time <- as.numeric(time$toc - time$tic)
  result <- appendNewTime(result, time, tasks[3])

  # covariate
  tictoc::tic()
  cdm[[studyCohort]] %>%
    PatientProfiles::summariseCharacteristics(cohortIntersect = list(
      "Occurrence in previous year" = list(
        targetCohortTable = studyCohort, window = c(-365, 0), value = "flag"
      )
    ), tableIntersect = list()) %>%
    invisible()
  time <- tictoc::toc(quiet = TRUE)
  time <- as.numeric(time$toc - time$tic)
  result <- appendNewTime(result, time, tasks[4])

  # all
  tictoc::tic()
  cdm[[studyCohort]] %>%
    PatientProfiles::summariseCharacteristics(
      ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150)),
      tableIntersect = list("Visits in previous year" = list(
        tableName = "visit_occurrence", window = c(-365, 0), value = "count"
      )),
      cohortIntersect = list("Occurrence in previous year" = list(
        targetCohortTable = studyCohort, window = c(-365, 0), value = "flag"
      ))
    ) %>%
    invisible()
  time <- tictoc::toc(quiet = TRUE)
  time <- as.numeric(time$toc - time$tic)
  result <- appendNewTime(result, time, tasks[5])

  # summary
  result <- groupTotal(result, "TOTAL summariseCharacteristics", tasks)

  return(result)
}
benchmarkSummariseLargeScaleCharacteristics <- function(result, cdm, studyCohort) {
  tasks <- c(
    "lsc condition_occurrence standard", "lsc drug_exposure standard",
    "lsc icd10 subchapter", "lsc atc 3rd", "lsc condition_occurrence source",
    "lsc all"
  )

  # condition_occurrence
  tictoc::tic()
  cdm[[studyCohort]] %>%
    PatientProfiles::summariseLargeScaleCharacteristics(
      eventInWindow = "condition_occurrence"
    ) %>%
    invisible()
  time <- tictoc::toc(quiet = TRUE)
  time <- as.numeric(time$toc - time$tic)
  result <- appendNewTime(result, time, tasks[1])

  # drug_exposure
  tictoc::tic()
  cdm[[studyCohort]] %>%
    PatientProfiles::summariseLargeScaleCharacteristics(
      episodeInWindow = "drug_exposure"
    ) %>%
    invisible()
  time <- tictoc::toc(quiet = TRUE)
  time <- as.numeric(time$toc - time$tic)
  result <- appendNewTime(result, time, tasks[2])

  # icd10
  # tictoc::tic()
  # cdm[[studyCohort]] %>%
  #   PatientProfiles::summariseLargeScaleCharacteristics(
  #     eventInWindow = "ICD10 Sub-Chapter"
  #   ) %>%
  #   invisible()
  # time <- tictoc::toc(quiet = TRUE)
  # time <- as.numeric(time$toc - time$tic)
  # result <- appendNewTime(result, time, tasks[3])

  # atc
  tictoc::tic()
  cdm[[studyCohort]] %>%
    PatientProfiles::summariseLargeScaleCharacteristics(
      episodeInWindow = "ATC 3rd",
    ) %>%
    invisible()
  time <- tictoc::toc(quiet = TRUE)
  time <- as.numeric(time$toc - time$tic)
  result <- appendNewTime(result, time, tasks[4])

  # source
  tictoc::tic()
  cdm[[studyCohort]] %>%
    PatientProfiles::summariseLargeScaleCharacteristics(
      eventInWindow = "condition_occurrence", includeSource = TRUE
    ) %>%
    invisible()
  time <- tictoc::toc(quiet = TRUE)
  time <- as.numeric(time$toc - time$tic)
  result <- appendNewTime(result, time, tasks[5])

  # ls all
  tictoc::tic()
  cdm[[studyCohort]] %>%
    PatientProfiles::summariseLargeScaleCharacteristics(
      eventInWindow = c("condition_occurrence"),
      episodeInWindow = c("drug_exposure", "ATC 3rd"),
      includeSource = TRUE
    ) %>%
    invisible()
  time <- tictoc::toc(quiet = TRUE)
  time <- as.numeric(time$toc - time$tic)
  result <- appendNewTime(result, time, tasks[6])

  # summary
  result <- groupTotal(result, "TOTAL lsc", tasks)

  return(result)
}
