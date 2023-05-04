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
#'
#' @param cdm 'cdm' object created with CDMConnector::cdm_from_con(). It must
#' must contain the 'targetCohort' table and all the tables that we want to
#' characterize. It is a compulsory input, no default value is provided.
#' @param targetCohortName Name of the table in the cdm that contains the
#' target cohort that we want to characterize. It is a compulsory input, no
#' default value is provided.
#' @param targetCohortId Cohort definition id for the analyzed target cohorts.
#' It can be a vector or a number. If it is NULL all cohorts are analyzed. By
#' default: NULL.
#' @param temporalWindows Temporal windows that we want to characterize. It must
#' be a list of numeric vectors of length two. The tables will be characterized
#' between the first element and the second element respect to the
#' cohort_start_date of each individual. To refer to any time prior set NA the
#' first element of the vector. To refer to any time after the index date set NA
#' the second element of the vector. By default: list(c(-Inf, -366), c(-365, -90),
#' c(-365, -31), c(-90, -1), c(-30, -1), c(0, 0), c(1, 30), c(1, 90),
#' c(31, 365), c(91, 365), c(366, Inf)).
#' @param tablesToCharacterize Name of the tables in the cdm that we want to
#' summarize. The available tables to characterize are: "visit_occurrence",
#' "condition_occurrence", "drug_exposure", "procedure_occurrence",
#' "device_exposure", "measurement", "observation", "drug_era", "condition_era"
#' and "specimen". By default: c("condition_occurrence", "drug_era",
#' "procedure_occurrence", "measurement").
#' @param overlap Whether you want to consider overlapping events (overlap =
#' TRUE) or only incident ones (overlap = FALSE).
#' @return The output of this function is a table containing summary characteristics.
#' Key information like temporalWindows considered will be output in columns
#' window_id and window_name. tablesToChacaterize will be output in columns
#' table_id and table_name. Concept information is output in columns
#' concept_id, concept_name, concept_type and concept_count. Denominator information
#' denominator_count is also reported. "overlap" setting is also included
#' in the table for reference.
#'
#' @export
#' @examples
#' \dontrun{
#' library(DBI)
#' library(duckdb)
#' library(LargeScaleCharacteristics)
#' cdm <- mockLargeScaleCharacteristics()
#' getLargeScaleCharacteristics(cdm, targetCohortName = c("cohort1"))
#' }
getLargeScaleCharacteristics <- function(cdm,
                                         targetCohortName,
                                         targetCohortId = NULL,
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
                                         overlap = TRUE) {



  parameters <- checkCohort(cdm, targetCohortId, targetCohortName)

  targetCohortId <- parameters$targetCohortId

  targetCohort <- parameters$targetCohort

  checkCdm(cdm, tablesToCharacterize)

  # write temporal windows tibble
  temporalWindows <- checkWindow(temporalWindows)
  #check tablesToCharacterize
  checkTable(cdm, tablesToCharacterize)
  # overlap
  overlap <- checkOverlap(overlap, tablesToCharacterize)
  # get the distinct subjects with their observation period
  denominatork <- calculateDenominators(cdm,
                                        targetCohortName,
                                        targetCohortId,
                                        targetCohort,
                                        temporalWindows)


  subjects <- getSubjects(cdm, targetCohort)

  # get the codes observed in each window for each one of the subjects, only
  # events in the observation window will be observed. The result is a
  # temporary table in the database
  characterizedTable <- lapply(tablesToCharacterize, function(table_name) {
    study_table <- subSetTable(cdm[[table_name]], subjects)

    overlap.k <- overlap[tablesToCharacterize == table_name]

    getCounts(cdm, study_table, table_name, overlap.k, temporalWindows)
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
  for (k in 1:length(targetCohortId)) {
    characterizedCohort <- targetCohort %>%
      dplyr::filter(.data$cohort_definition_id == !!targetCohortId[k]) %>%
      dplyr::select(
        "person_id" = "subject_id", "cohort_start_date", "cohort_end_date"
      ) %>%
      dplyr::inner_join(
        characterizedTables,
        by = c("person_id", "cohort_start_date", "cohort_end_date")
      ) %>%
      dplyr::group_by(.data$concept_id, .data$concept_name, .data$window_id, .data$table_id, .data$table_name) %>%
      dplyr::tally() %>%
      dplyr::ungroup() %>%
      dplyr::rename("concept_count" = "n") %>%
      dplyr::collect() %>%
      dplyr::mutate(cohort_definition_id = targetCohortId[k])
    if (k == 1) {
      characterizedCohortk <- characterizedCohort
    } else {
      characterizedCohortk <- characterizedCohortk %>%
        dplyr::union_all(characterizedCohort)
    }
  }

  tablesToCharacterize <- tibble::tibble(
    table_id = seq(length(tablesToCharacterize)),
    table_name = tablesToCharacterize,
    overlap = overlap
  )

  characterizedTables <- characterizedCohortk %>%
    dplyr::relocate("cohort_definition_id", .before = "concept_id") %>%
    dplyr::select(
      "cohort_definition_id", "table_id", "window_id", "concept_id",
      "concept_name", "concept_count", "table_name"
    )


  result <- characterizedTables %>%
    dplyr::left_join(tablesToCharacterize, by = c("table_id", "table_name")) %>%
    dplyr::left_join(denominatork,
      by = c(
        "window_id", "cohort_definition_id"
      )
    ) %>%
    dplyr::left_join(temporalWindows, by = "window_id") %>%
    dplyr::select(
      "cohort_definition_id", "table_id", "table_name",
      "window_id", "window_name", "concept_id",
      "concept_name", "concept_count", "denominator_count",
      "overlap"
    ) %>%
    dplyr::mutate(concept_type = "Standard")


  result <- result %>%
    dplyr::mutate(cdm_name = CDMConnector::cdmName(.env$cdm)) %>%
    dplyr::mutate(cohort_name = .env$targetCohortName) %>%
    dplyr::select("cohort_definition_id", "cohort_name", "table_name", "window_name", "concept_id",
                  "concept_name", "concept_count", "denominator_count",
                  "overlap", "concept_type", "cdm_name"
    )

  return(result)
}
