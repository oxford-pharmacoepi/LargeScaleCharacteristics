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
                                           c(NA, -366), c(-365, -91),
                                           c(-365, -31), c(-90, -1), c(-30, -1),
                                           c(0, 0), c(1, 30), c(1, 90),
                                           c(31, 365), c(91, 365), c(366, NA)
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

  errorMessage <- checkmate::makeAssertCollection()

  # check cdm
  checkmate::assertClass(cdm, "cdm_reference", add = errorMessage)

  # check x
  xCheck <- inherits(x, "tbl_dbi")
  if (!isTRUE(xCheck)) {
    errorMessage$push(
      "- x is not a table"
    )
  }


  # check that x has tables required
  checkmate::assertTRUE(
    all(c(
      "cohort_definition_id",
      "subject_id",
      "cohort_start_date",
      "cohort_end_date"
    ) %in% colnames(x)),
    add = errorMessage
  )

  # check input cohort cannot have missing in the following columns
  checkmate::assertTRUE(
    !checkmate::anyMissing(x %>% dplyr::pull("cohort_definition_id")),
    add = errorMessage
  )

  checkmate::assertTRUE(
    !checkmate::anyMissing(x %>% dplyr::pull("subject_id")),
    add = errorMessage
  )

  checkmate::assertTRUE(
    !checkmate::anyMissing(x %>% dplyr::pull("cohort_start_date")),
    add = errorMessage
  )

  checkmate::assertTRUE(
    !checkmate::anyMissing(x %>% dplyr::pull("cohort_end_date")),
    add = errorMessage
  )


  # check temporalWindows
  checkmate::assertList(temporalWindows, min.len = 1, add = errorMessage)
  checkmate::assertTRUE(
    all(unlist(lapply(temporalWindows, length)) == 2),
    add = errorMessage
  )

  # check tablesToCharacterize
  checkmate::assertCharacter(
    tablesToCharacterize,
    min.len = 1,
    add = errorMessage
  )
  checkmate::assertTRUE(
    all(tablesToCharacterize %in% names(cdm)),
    add = errorMessage
  )
  checkmate::assertTRUE(
    all(tablesToCharacterize %in% c(
      "visit_occurrence", "condition_occurrence", "drug_exposure",
      "procedure_occurrence", "device_exposure", "measurement", "observation",
      "drug_era", "condition_era", "specimen"
    )),
    add = errorMessage
  )

  # overlap
  checkmate::assertLogical(overlap, any.missing = FALSE, add = errorMessage)

  # minimumFrequency
  checkmate::assert_numeric(minimumFrequency, lower = 0, upper = 1, add = errorMessage)

  # report collection of errors
  checkmate::reportAssertions(collection = errorMessage)

  if (length(overlap) > 1) {
    if (length(overlap) != length(tablesToCharacterize)) {
      stop("If length(overlap)>1 then length(overlap) = length(tablesToCharacterize)")
    }
  } else {
    overlap <- rep(overlap, length(tablesToCharacterize))
  }

  # write temporal windows tibble
  temporalWindows <- lapply(temporalWindows, function(x) {
    nam <- paste0(
      ifelse(is.na(x[1]), "Any", x[1]),
      ";",
      ifelse(is.na(x[2]), "Any", x[2])
    )
    x <- dplyr::tibble(
      window_start = x[1], window_end = x[2], window_name = nam
    )
    return(x)
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(window_id = dplyr::row_number()) %>%
    dplyr::select("window_id", "window_name", "window_start", "window_end")



  # get the distinct subjects with their observation period
  subjects <- x %>%
    dplyr::select(
      "person_id" = "subject_id",
      "cohort_start_date",
      "cohort_end_date"
    ) %>%
    dplyr::distinct() %>%
    dplyr::left_join(
      cdm[["observation_period"]] %>%
        dplyr::select(
          "person_id",
          "observation_period_start_date",
          "observation_period_end_date"
        ),
      by = "person_id"
    )

  characterizedTable <- lapply(tablesToCharacterize, function(table_name) {
    overlap.k <- overlap[tablesToCharacterize == table_name]
    # get start date depending on the table
    start_date <- get_start_date[[table_name]]
    # get end date depending on the table
    end_date <- get_end_date[[table_name]]
    # get concept id depending on the table
    concept_id <- get_concept[[table_name]]
    # subset the table to the study subjects
    study_table <- cdm[[table_name]] %>%
      dplyr::inner_join(subjects, by = "person_id") %>%
      # rename start date
      dplyr::rename("start_date" = .env$start_date)
    # rename or create end date
    if (is.null(end_date) || isFALSE(overlap.k)) {
      study_table <- study_table %>%
        dplyr::mutate(end_date = .data$start_date)
    } else {
      study_table <- study_table %>%
        dplyr::rename("end_date" = .env$end_date)
    }
    study_table <- study_table %>%
      # rename concept id and get concept name
      dplyr::rename("concept_id" = .env$concept_id) %>%
      dplyr::mutate(table_name = table_name) %>%
      dplyr::filter(concept_id != 0) %>%
      dplyr::left_join(
        cdm$concept %>%
          dplyr::select("concept_id", "concept_name"),
        by = "concept_id"
      ) %>%
      # obtain observations inside the observation period only
      dplyr::mutate(flag = dplyr::if_else(.data$start_date <= .data$observation_period_end_date, 1, 0)) %>%
      dplyr::mutate(flag = dplyr::if_else(.data$end_date >= .data$observation_period_start_date, .data$flag, 0)) %>%
      # obtain the time difference between the start of the event and the
      # cohort start date
      dplyr::mutate(days_difference_start = dbplyr::sql(CDMConnector::datediff(
        start = "cohort_start_date",
        end = "start_date"
      )))
    # obtain the time difference between the end of the event and the cohort
    # start date
    if (is.null(end_date) || isFALSE(overlap.k)) {
      study_table <- study_table %>%
        dplyr::mutate(days_difference_end = .data$days_difference_start)
    } else {
      study_table <- study_table %>%
        dplyr::mutate(days_difference_end = dbplyr::sql(CDMConnector::datediff(
          start = "cohort_start_date",
          end = "end_date"
        )))
    }
    study_table <- study_table %>%
      # merge the table that we want to characterize with all the temporal
      # windows
      dplyr::mutate(to_merge = 1) %>%
      dplyr::inner_join(
        temporalWindows %>%
          dplyr::mutate(to_merge = 1),
        by = "to_merge",
        copy = TRUE
      ) %>%
      # get only the events that start before the end of the window
      dplyr::mutate(flag = dplyr::if_else(.data$flag != 0 & (is.na(
        .data$window_end
      ) | .data$days_difference_start <= .data$window_end), 1, 0)) %>%
      # get only events that end/start (depending if overlap = TRUE/FALSE) after
      # the start of the window
      dplyr::mutate(flag = dplyr::if_else(.data$flag != 0 & (is.na(
        .data$window_start
      ) | .data$days_difference_end >= .data$window_start), .data$flag, 0)) %>%
      # get only distinct events per window id
      dplyr::select(
        "person_id", "cohort_start_date", "cohort_end_date", "window_id",
        "concept_id", "concept_name", "table_name", "flag"
      ) %>%
      dplyr::distinct() %>%
      dplyr::compute()

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
