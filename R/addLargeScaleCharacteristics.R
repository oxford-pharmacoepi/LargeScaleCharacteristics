# Copyright 2023 OXINFER (C)
#
# This file is part of LargeScaleCharacteristics package
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

#' Explain function
#' @param x table
#' @param cdm 'cdm' object created with CDMConnector::cdm_from_con(). It must
#' must contain the input x table and all the tables that we want to
#' characterize. It is a compulsory input, no default value is provided.
#' @param temporalWindows Temporal windows that we want to characterize. It must
#' be a list of numeric vectors of length two. The tables will be characterized
#' between the first element and the second element respect to the
#' cohort_start_date of each individual. To refer to any time prior set NA the
#' first element of the vector. To refer to any time after the index date set NA
#' the second element of the vector. By default: list(c(NA, -366), c(-365, -90),
#' c(-365, -31), c(-90, -1), c(-30, -1), c(0, 0), c(1, 30), c(1, 90),
#' c(31, 365), c(91, 365), c(366, NA)).
#' @param tablesToCharacterize Name of the tables in the cdm that we want to
#' summarize. The available tables to characterize are: "visit_occurrence",
#' "condition_occurrence", "drug_exposure", "procedure_occurrence",
#' "device_exposure", "measurement", "observation", "drug_era", "condition_era"
#' and "specimen". By default: c("condition_occurrence", "drug_era",
#' "procedure_occurrence", "measurement").
#' @param overlap Whether you want to consider overlapping events (overlap =
#' TRUE) or only incident ones (overlap = FALSE).
#' @param minimumFrequency the minimum frequency for concept id to be included in output. Can
#' take a value between 0 and 1. By default: 0.
#'
#' @return The output of this function is a 3 elements list. First
#' ("Characterization") is a reference to a temporal table in the database. It
#' contains the characterization of the desired cohorts of interest. The cohorts
#' of interest are specified using 'targetCohortId' and 'targetCohortName'. The
#' characterized tables are the ones specified in 'tablesToChacaterize'. Second
#' ("temporalWindows") contains the windows used to do the characaterization.
#' Finally "overlap" is also included in the list.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(DBI)
#' library(duckdb)
#' library(LargeScaleCharacteristics)
#' cdm <- mockLargeScaleCharacteristics()
#' addLargeScaleCharacteristics(cdm$cohort1, cdm)
#' }
addLargeScaleCharacteristics <- function(x,
                                         cdm,
                                         temporalWindows = list(
                                           c(-Inf, -366), c(-365, -91),
                                           c(-365, -31), c(-90, -1), c(-30, -1),
                                           c(0, 0), c(1, 30), c(1, 90),
                                           c(31, 365), c(91, 365), c(366, Inf)
                                         ),
                                         tablesToCharacterize = c(
                                           "condition_occurrence", "drug_era",
                                           "procedure_occurrence", "measurement"
                                         ),
                                         overlap = TRUE,
                                         minimumFrequency = 0) {
  get_start_date <- list(
    "visit_occurrence" = "visit_start_date",
    "condition_occurrence" = "condition_start_date",
    "drug_exposure" = "drug_exposure_start_date",
    "procedure_occurrence" = "procedure_date",
    "device_exposure" = "device_exposure_start_date",
    "measurement" = "measurement_date",
    "observation" = "observation_date",
    "drug_era" = "drug_era_start_date",
    "condition_era" = "condition_era_start_date",
    "specimen" = "specimen_date"
  )

  get_end_date <- list(
    "visit_occurrence" = "visit_end_date",
    "condition_occurrence" = "condition_end_date",
    "drug_exposure" = "drug_exposure_end_date",
    "procedure_occurrence" = NULL,
    "device_exposure" = "device_exposure_end_date",
    "measurement" = NULL,
    "observation" = NULL,
    "drug_era" = "drug_era_end_date",
    "condition_era" = "condition_era_end_date",
    "specimen" = NULL
  )

  get_concept <- list(
    "visit_occurrence" = "visit_concept_id",
    "condition_occurrence" = "condition_concept_id",
    "drug_exposure" = "drug_concept_id",
    "procedure_occurrence" = "procedure_concept_id",
    "device_exposure" = "device_concept_id",
    "measurement" = "measurement_concept_id",
    "observation" = "observation_concept_id",
    "drug_era" = "drug_concept_id",
    "condition_era" = "condition_concept_id",
    "specimen" = "specimen_concept_id"
  )


  # check cdm
  checkCdm(cdm, tablesToCharacterize)

  # check x
  checkX(x)

  #check tablesToCharacterize
  checkTable(cdm, tablesToCharacterize)

  temporalWindows <- checkWindow(temporalWindows)

  # minimumFrequency
  checkmate::assert_numeric(minimumFrequency, lower = 0, upper = 1)

  # overlap
  overlap <- checkOverlap(overlap, tablesToCharacterize)

  # get the distinct subjects with their observation period
  subjects <- getSubjects(cdm, x)

  characterizedTable <- lapply(tablesToCharacterize, function(table_name) {
    study_table <- subSetTable(cdm[[table_name]], subjects)

    overlap.k <- overlap[tablesToCharacterize == table_name]

    study_table <- addFlag(cdm, study_table, table_name, overlap.k, temporalWindows)

    return(study_table)
  })


  # union all the tables into a temporal table
  for (i in 1:length(characterizedTable)) {
    if (i == 1) {
      characterizedTables <- characterizedTable[[i]] %>%
        dplyr::mutate(table_id = .env$i)
    } else {
      characterizedTables <- characterizedTables %>%
        dplyr::union_all(
          characterizedTable[[i]] %>%
            dplyr::mutate(table_id = .env$i)
        )
    }
  }

  characterizedTables <- characterizedTables %>% dplyr::compute()

  # if we want to summarise the data we count the number of counts for each
  # event, window and table
  characterizedCohort <- x %>%
    dplyr::select(
      "person_id" = "subject_id", "cohort_start_date", "cohort_end_date"
    ) %>%
    dplyr::inner_join(
      characterizedTables,
      by = c("person_id", "cohort_start_date", "cohort_end_date")
    ) %>%
    dplyr::compute()

  n_subjects <- subjects %>% dplyr::count() %>% dplyr::pull()

  countConcepts <- characterizedCohort %>%
    dplyr::select("person_id", "window_id", "concept_id", "table_id", "flag") %>%
    dplyr::filter(.data$flag != 0) %>%
    dplyr::group_by(.data$window_id, .data$concept_id, .data$table_id) %>%
    dplyr::summarise('conceptCount'=sum(.data$flag, na.rm = TRUE), .groups = "keep") %>%
    dplyr::ungroup() %>%
    dplyr::right_join(characterizedCohort, by = c( "window_id", "concept_id", "table_id")) %>%
    dplyr::mutate(conceptCount = ifelse(is.na(.data$conceptCount), 0, .data$conceptCount)) %>%
    dplyr::mutate(freq = .data$conceptCount/n_subjects) %>%
    dplyr::filter(.data$freq >= .env$minimumFrequency) %>%
    dplyr::select("person_id", "window_id", "concept_id", "table_id")


  characterizedCohort <- characterizedCohort %>%
    dplyr::inner_join(countConcepts,by = c( "person_id", "window_id", "concept_id", "table_id"))

  characterizedCohortk_spread <- characterizedCohort %>%
    dplyr::left_join(temporalWindows %>% dplyr::select("window_id", "window_name"),
                     by = "window_id",
                     copy = TRUE
    ) %>%
    dplyr::select(-c("window_id", "concept_name", "table_id")) %>%
    tidyr::pivot_wider(
      names_from = c(table_name, concept_id, window_name),
      names_glue = "{table_name}_{concept_id}_{window_name}",
      values_from = flag
    )

  result <- subjects %>%
    dplyr::left_join(characterizedCohortk_spread,
                     by = c("person_id", "cohort_start_date", "cohort_end_date")
    ) %>%
    dplyr::rename("subject_id" = "person_id") %>%
    dplyr::select(-c("observation_period_start_date", "observation_period_end_date")) %>%
    dplyr::compute()

  result <- result %>% replace(is.na(.), 0)

  return(result)
}
