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
#' @param includeDescendants A vector of whether to include descendant or not,
#' for each tablesToCharacterize, default as all FALSE
#' @param includeSources A vector of whether to include source id or not,
#' for each tablesToCharacterize, default as all FALSE
#' @param overlap Whether you want to consider overlapping events (overlap =
#' TRUE) or only incident ones (overlap = FALSE).
#' @param smd Whether you want to compute SMD, default set to false
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
                                         includeDescendants = FALSE,
                                         includeSources = FALSE,
                                         overlap = TRUE,
                                         smd = FALSE) {
  parameters <- checkCohort(cdm, targetCohortId, targetCohortName)

  targetCohortId <- parameters$targetCohortId

  targetCohort <- parameters$targetCohort

  checkCdm(cdm, tablesToCharacterize)

  # write temporal windows tibble
  temporalWindows <- checkWindow(temporalWindows)
  # check tablesToCharacterize
  checkTable(cdm, tablesToCharacterize)
  # overlap
  overlap <- checkLogical(overlap, tablesToCharacterize)
  # get the distinct subjects with their observation period
  denominatork <- calculateDenominators(
    cdm,
    targetCohortName,
    targetCohortId,
    targetCohort,
    temporalWindows
  )


  includeDescendants <- checkLogical(includeDescendants, tablesToCharacterize)

  includeSources <- checkLogical(includeSources, tablesToCharacterize)

  tablesToCharacterize <- tibble::tibble(
    table_id = seq(length(tablesToCharacterize)),
    table_name = tablesToCharacterize,
    overlap = overlap,
    includeDescendants = includeDescendants,
    includeSources = includeSources
  )

  # get all tables for all tables to characterize
  # union all the tables into a temporal table
  subsetedTable <- NULL
  for (i in 1:length(tablesToCharacterize$table_name)) {
    subsetedTable[[i]] <- subSetTable(
      cdm, tablesToCharacterize, i,
      temporalWindows, targetCohort
    )
    if (i == 1) {
      subsetedTables <- subsetedTable[[i]] %>%
        dplyr::mutate(table_id = .env$i) %>%
        dplyr::compute()
    } else {
      subsetedTables <- subsetedTables %>%
        dplyr::union_all(
          subsetedTable[[i]] %>%
            dplyr::mutate(table_id = .env$i)
        ) %>%
        dplyr::compute()
    }
  }


  characterizedTables <- getCounts(cdm, targetCohort, targetCohortId, subsetedTables)


  # if we want to summarise the data we count the number of counts for each
  # event, window and table
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
      "overlap", "concept_type", "includeDescendants", "includeSources"
    )


  result <- result %>%
    dplyr::mutate(cdm_name = CDMConnector::cdmName(.env$cdm)) %>%
    dplyr::left_join(attr(cdm[[targetCohortName]],"cohort_set"), by = "cohort_definition_id") %>%
    # dplyr::mutate(cohort_name = attr(cdm[[targetCohortName]],"cohort_set")$cohort_name) %>%
    dplyr::select(
      "cohort_definition_id", "cohort_name", "table_name", "window_name", "concept_id",
      "concept_name", "concept_count", "denominator_count",
      "overlap", "concept_type", "includeDescendants", "includeSources", "cdm_name"
    )

  if (isTRUE(smd)) {
    result <- computeSmd(result)
    }

  return(result)
}
