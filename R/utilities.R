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

createCohort <- function(cdm,
                         prevalence,
                         seed) {
  set.seed(seed)
  nIndividuals <- cdm$person %>%
    dplyr::tally() %>%
    dplyr::pull("n") * prevalence %>%
    round()
  ind <- cdm$person %>%
    dplyr::pull("person_id") %>%
    sample(nIndividuals)
  cohortRef <- cdm$observation_period %>%
    dplyr::filter(.data$person_id %in% .env$ind) %>%
    dplyr::select(
      "subject_id" = "person_id",
      "cohort_start_date" = "observation_period_start_date",
      "cohort_end_date" = "observation_period_end_date"
    ) %>%
    dplyr::mutate(cohort_definition_id = 1) %>%
    dplyr::select(
      "cohort_definition_id", "subject_id", "cohort_start_date",
      "cohort_end_date"
    ) %>%
    CDMConnector::computeQuery()
  cohort <- CDMConnector::newGeneratedCohortSet(cohortRef = cohortRef)
  return(cohort)
}

correctMock <- function(cdm) {
  cdm$cdm_source <- dplyr::tibble(
    cdm_source_name = "PP", cdm_source_abbreviation = "PP",
    cdm_holder = "OMOP", source_description = "www",
    source_documentation_reference = "wwww", cdm_etl_reference = "www",
    source_release_date = as.Date("2020-01-01"),
    cdm_release_date = as.Date("2021-01-01"), cdm_version = "v5.3",
    vocabulary_version = "v5.0 22-JUN-22"
  )
  cdm$vocabulary <- dplyr::tibble(
    vocabulary_id = "None", vocab_version = "v5.3",
    vocabulary_name = as.character(NA), vocabulary_reference = as.character(NA),
    vocabulary_version = as.character(NA),
    vocabulary_concept_id = as.character(NA)
  )
  return(cdm)
}
