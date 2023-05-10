getSubjects <- function(cdm,
                        targetCohort,
                        all = TRUE) {
  subjects <- targetCohort %>%
    dplyr::select(
      "person_id" = "subject_id",
      "cohort_start_date",
      "cohort_end_date"
    ) %>%
    dplyr::distinct() %>%
    dplyr::inner_join(
      cdm[["observation_period"]] %>%
        dplyr::select(
          "person_id",
          "observation_period_start_date",
          "observation_period_end_date"
        ),
      by = "person_id"
    ) %>%
    dplyr::compute()

  return(subjects)
}


namesTable <- readr::read_csv(
  here("extras", "namesTable.csv"),
  col_types = list(
    table_name = "c",
    start_date_name = "c",
    end_date_name = "c",
    concept_id_name = "c"
  )
)


subSetTable <- function(cdm, tablesToCharacterize, i, windows, targetCohort, overlap) {

    table_name <- tablesToCharacterize$table_name[i]

    subjects <- getSubjects(cdm, targetCohort)

    subsetedTable <- cdm[[table_name]] %>%
      dplyr::inner_join(subjects, by = "person_id")

    overlap.k <- overlap[tablesToCharacterize == table_name]

    start_date <- namesTable$start_date_name[namesTable$table_name == table_name]
    # get end date depending on the table
    end_date <- namesTable$end_date_name[namesTable$table_name == table_name]
    # get concept id depending on the table
    concept_id <- namesTable$concept_id_name[namesTable$table_name == table_name]

    subsetedTable <- subsetedTable %>%
      # rename start date
      dplyr::rename("start_date" = .env$start_date)

    # rename or create end date
    if (is.na(end_date) || isFALSE(overlap)) {
      subsetedTable <- subsetedTable %>%
        dplyr::mutate(end_date = .data$start_date)
    } else {
      subsetedTable <- subsetedTable %>%
        dplyr::rename("end_date" = .env$end_date)
    }
    subsetedTable <- subsetedTable %>%
      # rename concept id and get concept name
      dplyr::rename("concept_id" = .env$concept_id) %>%
      dplyr::left_join(
        cdm$concept %>%
          dplyr::select("concept_id", "concept_name"),
        by = "concept_id"
      ) %>%
      # obtain observations inside the observation period only
      dplyr::filter(.data$start_date <= .data$observation_period_end_date) %>%
      dplyr::filter(.data$end_date >= .data$observation_period_start_date) %>%
      # obtain the time difference between the start of the event and the
      # cohort start date
      dplyr::mutate(days_difference_start = dbplyr::sql(CDMConnector::datediff(
        start = "cohort_start_date",
        end = "start_date"
      )))

    # obtain the time difference between the end of the event and the cohort
    # start date
    if (is.na(end_date) || isFALSE(overlap)) {
      subsetedTable <- subsetedTable %>%
        dplyr::mutate(days_difference_end = .data$days_difference_start)
    } else {
      subsetedTable <- subsetedTable %>%
        dplyr::mutate(days_difference_end = dbplyr::sql(CDMConnector::datediff(
          start = "cohort_start_date",
          end = "end_date"
        )))
    }

    subsetedTable <- subsetedTable %>%
      # merge the table that we want to characterize with all the temporal
      # windows
      dplyr::mutate(to_merge = 1) %>%
      dplyr::inner_join(
        windows %>%
          dplyr::mutate(to_merge = 1),
        by = "to_merge",
        copy = TRUE
      ) %>%
      # get only the events that start before the end of the window
      dplyr::filter(
        is.infinite(.data$upper) |
          .data$days_difference_start <= .data$upper
      ) %>%
      # get only events that end/start (depending if overlap = TRUE/FALSE) after
      # the start of the window
      dplyr::filter(
        is.infinite(.data$lower) |
          .data$days_difference_end >= .data$lower
      ) %>%
      # get only distinct events per window id
      dplyr::select(
        "person_id", "cohort_start_date", "cohort_end_date", "window_id",
        "concept_id", "concept_name"
      ) %>%
      dplyr::distinct() %>%
      dplyr::mutate(table_name = table_name) %>%
      dplyr::compute()

  return(subsetedTable)
}

getCounts <- function(cdm, targetCohort, targetCohortId, characterizedTables) {
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



  characterizedTables <- characterizedCohortk %>%
    dplyr::relocate("cohort_definition_id", .before = "concept_id") %>%
    dplyr::select(
      "cohort_definition_id", "table_id", "window_id", "concept_id",
      "concept_name", "concept_count", "table_name"
    )

  return(characterizedTables)
}


calculateDenominators <- function(cdm,
                                  targetCohortName,
                                  targetCohortId = NULL,
                                  targetCohort,
                                  temporalWindows = list(
                                    c(-Inf, -366), c(-365, -91),
                                    c(-365, -31), c(-90, -1), c(-30, -1),
                                    c(0, 0), c(1, 30), c(1, 90),
                                    c(31, 365), c(91, 365), c(366, Inf)
                                  )) {
  subjects <- getSubjects(cdm, targetCohort)

  subjects_denominator <- subjects %>%
    dplyr::mutate(dif_start = dbplyr::sql(CDMConnector::datediff(
      start = "cohort_start_date",
      end = "observation_period_start_date"
    ))) %>%
    dplyr::mutate(dif_end = dbplyr::sql(CDMConnector::datediff(
      start = "cohort_start_date",
      end = "observation_period_end_date"
    ))) %>%
    dplyr::mutate(to_merge = 1) %>%
    dplyr::inner_join(
      temporalWindows %>%
        dplyr::mutate(to_merge = 1),
      by = "to_merge",
      copy = TRUE
    ) %>%
    dplyr::filter(
      is.na(.data$upper) | .data$dif_start <= .data$upper
    ) %>%
    dplyr::filter(
      is.na(.data$lower) | .data$dif_end >= .data$lower
    ) %>%
    dplyr::select(
      "person_id", "cohort_start_date", "cohort_end_date", "window_id"
    ) %>%
    dplyr::compute()

  for (k in 1:length(targetCohortId)) {
    denominator <- targetCohort %>%
      dplyr::rename("person_id" = "subject_id") %>%
      dplyr::filter(.data$cohort_definition_id == !!targetCohortId[k]) %>%
      dplyr::inner_join(
        subjects_denominator,
        by = c("person_id", "cohort_start_date", "cohort_end_date")
      ) %>%
      dplyr::group_by(.data$window_id) %>%
      dplyr::tally() %>%
      dplyr::ungroup() %>%
      dplyr::rename("denominator_count" = "n") %>%
      dplyr::collect() %>%
      dplyr::mutate(cohort_definition_id = targetCohortId[k])

    if (k == 1) {
      denominatork <- denominator
    } else {
      denominatork <- denominatork %>%
        dplyr::union_all(denominator)
    }
  }

  denominatork <- denominatork %>%
    dplyr::select("window_id", "denominator_count", "cohort_definition_id")


  return(denominatork)
}





addFlag <- function(cdm, subjects, table_name, overlap, windows) {
  subsetedTable <- cdm[[table_name]] %>%
    dplyr::inner_join(subjects, by = "person_id")


  start_date <- namesTable$start_date_name[namesTable$table_name == table_name]
  # get end date depending on the table
  end_date <- namesTable$end_date_name[namesTable$table_name == table_name]
  # get concept id depending on the table
  concept_id <- namesTable$concept_id_name[namesTable$table_name == table_name]

  subsetedTable <- subsetedTable %>%
    # rename start date
    dplyr::rename("start_date" = .env$start_date)

  # rename or create end date
  if (is.na(end_date) || isFALSE(overlap)) {
    subsetedTable <- subsetedTable %>%
      dplyr::mutate(end_date = .data$start_date)
  } else {
    subsetedTable <- subsetedTable %>%
      dplyr::rename("end_date" = .env$end_date)
  }
  subsetedTable <- subsetedTable %>%
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
  if (is.na(end_date) || isFALSE(overlap)) {
    subsetedTable <- subsetedTable %>%
      dplyr::mutate(days_difference_end = .data$days_difference_start)
  } else {
    subsetedTable <- subsetedTable %>%
      dplyr::mutate(days_difference_end = dbplyr::sql(CDMConnector::datediff(
        start = "cohort_start_date",
        end = "end_date"
      )))
  }
  subsetedTable <- subsetedTable %>%
    # merge the table that we want to characterize with all the temporal
    # windows
    dplyr::mutate(to_merge = 1) %>%
    dplyr::inner_join(
      windows %>%
        dplyr::mutate(to_merge = 1),
      by = "to_merge",
      copy = TRUE
    ) %>%
    # get only the events that start before the end of the window
    dplyr::mutate(flag = dplyr::if_else(.data$flag != 0 & (is.na(
      .data$upper
    ) | .data$days_difference_start <= .data$upper), 1, 0)) %>%
    # get only events that end/start (depending if overlap = TRUE/FALSE) after
    # the start of the window
    dplyr::mutate(flag = dplyr::if_else(.data$flag != 0 & (is.na(
      .data$lower
    ) | .data$days_difference_end >= .data$lower), .data$flag, 0)) %>%
    # get only distinct events per window id
    dplyr::select(
      "person_id", "cohort_start_date", "cohort_end_date", "window_id",
      "concept_id", "concept_name", "table_name", "flag"
    ) %>%
    dplyr::distinct() %>%
    dplyr::compute()
  return(subsetedTable)
}






getDescendantCounts <- function(x, cdm) {
  x <- x %>%
    dplyr::inner_join(cdm$concept_ancestor) %>%
    dplyr::group_by(.data$cohort_name, .data$table_name, .data$window_name, .data$ancestor_concept_id) %>%
    dplyr::summarise(concept_count = sum(.data$concept_count)) %>%
    dplyr::rename("ancestor_concept_id" = "concept_id")

  return(x)
}
