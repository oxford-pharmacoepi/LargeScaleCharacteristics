# Copyright 2023 OXINFER (C)
#
# This file is part of LargeScaleCharacteristics
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

#' It creates a mock database for testing LargeScaleCharacteristics package
#' @param drug_exposure default null user can define its own table
#' @param drug_strength default null user can define its own table
#' @param observation_period default null user can define its own table
#' @param observation default null user can define its own observation table
#' @param condition_occurrence default null user can define its own table
#' @param visit_occurrence default null user can define its own visit_occurrence table
#' @param person default null user can define its own table
#' @param drug_concept_id_size number of unique drug concept id
#' @param measurement_size number of measurement
#' @param ingredient_concept_id_size number of unique drug ingredient concept id
#' @param procedure_occurrence_size number of procedure_occurrence
#' @param device_exposure_size number of device_exposure_size
#' @param drug_exposure_size number of unique drug exposure
#' @param patient_size number of unique patient
#' @param min_drug_exposure_start_date user define minimum drug exposure start date
#' @param max_drug_exposure_start_date user define maximium drug exposure start date
#' @param seed seed
#' @param condition_concept_id_size number of unique row in the condition concept table
#' @param visit_concept_id_size number of unique visit concept id
#' @param visit_occurrence_id_size number of unique visit occurrence id
#' @param earliest_date_of_birth the earliest date of birth of patient in person table format "dd-mm-yyyy"
#' @param latest_date_of_birth the latest date of birth for patient in person table format "dd-mm-yyyy"
#' @param earliest_observation_start_date the earliest observation start date for patient format "dd-mm-yyyy"
#' @param latest_observation_start_date the latest observation start date for patient format "dd-mm-yyyy"
#' @param min_days_to_observation_end the minimum number of days of the observational integer
#' @param max_days_to_observation_end the maximum number of days of the observation period integer
#' @param earliest_condition_start_date the earliest condition start date for patient format "dd-mm-yyyy"
#' @param earliest_visit_start_date the earliest visit start date for patient format "dd-mm-yyyy"
#' @param latest_condition_start_date the latest condition start date for patient format "dd-mm-yyyy"
#' @param latest_visit_start_date the latest visit start date for patient format "dd-mm-yyyy"
#' @param min_days_to_condition_end the minimum number of days of the condition integer
#' @param min_days_to_visit_end the minimum number of days of the visit integer
#' @param max_days_to_condition_end the maximum number of days of the condition integer
#' @param max_days_to_visit_end the maximum number of days of the visit integer
#' @param concept the concept table
#' @param measurement the measurement table
#' @param device_exposure the device_exposure table
#' @param procedure_occurrence the procedure_occurrence table
#' @param drug_era the drug_era table
#' @param specimen the specimen table
#' @param condition_era the condition_era table
#' @param specimen_concept_id_size the specimen_concept_id_size size
#' @param concept_ancestor the concept ancestor table
#' @param ancestor_concept_id_size the size of concept ancestor table
#' @param concept_id_size he size of concept table
#' @param cohort1 cohort table for test to run in getindication
#' @param cohort2 cohort table for test to run in getindication
#' @param ... user self defined tibble table to put in cdm, it can input as many as the user want
#' @return the function returns a mock database with OMOP tables that users/developers can test the functions on
#' @export
#' @examples
#' \dontrun{
#' library(DBI)
#' library(LargeScaleCharacteristics)
#' cdm <- mockLargeScaleCharacteristics()}
mockLargeScaleCharacteristics <- function(drug_exposure = NULL,
                                          drug_strength = NULL,
                                          observation_period = NULL,
                                          observation = NULL,
                                          condition_occurrence = NULL,
                                          visit_occurrence = NULL,
                                          concept_ancestor = NULL,
                                          concept = NULL,
                                          measurement = NULL,
                                          drug_era = NULL,
                                          condition_era = NULL,
                                          specimen = NULL,
                                          device_exposure = NULL,
                                          procedure_occurrence = NULL,
                                          person = NULL,
                                          cohort1 = NULL,
                                          cohort2 = NULL,
                                          drug_concept_id_size = 5,
                                          measurement_size = 5,
                                          concept_id_size = 5,
                                          ancestor_concept_id_size = 5,
                                          condition_concept_id_size = 5,
                                          specimen_concept_id_size = 5,
                                          visit_concept_id_size = 5,
                                          visit_occurrence_id_size = 5,
                                          ingredient_concept_id_size = 1,
                                          procedure_occurrence_size = 5,
                                          device_exposure_size = 5,
                                          drug_exposure_size = 10,
                                          patient_size = 5,
                                          min_drug_exposure_start_date = "2000-01-01",
                                          max_drug_exposure_start_date = "2020-01-01",
                                          earliest_date_of_birth = NULL,
                                          latest_date_of_birth = NULL,
                                          earliest_observation_start_date = NULL,
                                          latest_observation_start_date = NULL,
                                          min_days_to_observation_end = NULL,
                                          max_days_to_observation_end = NULL,
                                          earliest_condition_start_date = NULL,
                                          latest_condition_start_date = NULL,
                                          min_days_to_condition_end = NULL,
                                          max_days_to_condition_end = NULL,
                                          earliest_visit_start_date = NULL,
                                          latest_visit_start_date = NULL,
                                          min_days_to_visit_end = NULL,
                                          max_days_to_visit_end = NULL,
                                          seed = 1,
                                          ...) {
  # Put ... into a list
  listTables <- list(...)

  # checks
  errorMessage <- checkmate::makeAssertCollection()
  checkmate::assert_int(drug_exposure_size, lower = 1)
  checkmate::assert_int(patient_size, lower = 1)
  checkmate::assert_int(drug_concept_id_size, lower = 1)
  checkmate::assert_int(measurement_size, lower = 1)
  checkmate::assert_int(ancestor_concept_id_size, lower = 1)
  checkmate::assert_int(ingredient_concept_id_size, lower = 1)
  checkmate::assertTRUE(drug_exposure_size >= patient_size)
  checkmate::assert_tibble(person, null.ok = TRUE)
  checkmate::assert_tibble(measurement, null.ok = TRUE)
  checkmate::assert_tibble(drug_era, null.ok = TRUE)
  checkmate::assert_tibble(condition_era, null.ok = TRUE)
  checkmate::assert_tibble(specimen, null.ok = TRUE)
  checkmate::assert_tibble(procedure_occurrence, null.ok = TRUE)
  checkmate::assert_tibble(concept, null.ok = TRUE)
  checkmate::assert_tibble(observation_period, null.ok = TRUE)
  checkmate::assert_tibble(concept_ancestor, null.ok = TRUE)
  checkmate::assert_tibble(drug_exposure, null.ok = TRUE)
  checkmate::assert_tibble(condition_occurrence, null.ok = TRUE)
  checkmate::assert_tibble(visit_occurrence, null.ok = TRUE)
  checkmate::assert_tibble(drug_strength, null.ok = TRUE)
  checkmate::assert_int(seed, lower = 1)
  checkmate::assertDate(as.Date(earliest_date_of_birth), null.ok = TRUE)
  checkmate::assertDate(as.Date(latest_date_of_birth), null.ok = TRUE)
  checkmate::assertDate(as.Date(earliest_observation_start_date), null.ok = TRUE)
  checkmate::assertDate(as.Date(latest_observation_start_date), null.ok = TRUE)
  checkmate::assertDate(as.Date(earliest_condition_start_date), null.ok = TRUE)
  checkmate::assertDate(as.Date(latest_condition_start_date), null.ok = TRUE)
  checkmate::assertDate(as.Date(earliest_visit_start_date), null.ok = TRUE)
  checkmate::assertDate(as.Date(latest_visit_start_date), null.ok = TRUE)
  checkmate::assert_int(min_days_to_observation_end, lower = 1, null.ok = TRUE)
  checkmate::assert_int(max_days_to_observation_end, lower = 1, null.ok = TRUE)
  checkmate::assert_int(min_days_to_condition_end, lower = 1, null.ok = TRUE)
  checkmate::assert_int(max_days_to_condition_end, lower = 1, null.ok = TRUE)
  checkmate::assert_int(min_days_to_visit_end, lower = 1, null.ok = TRUE)
  checkmate::assert_int(max_days_to_visit_end, lower = 1, null.ok = TRUE)
  if (!is.null(latest_date_of_birth) &
    !is.null(earliest_date_of_birth)) {
    checkmate::assertTRUE(latest_date_of_birth >= earliest_date_of_birth)
  }
  if (!is.null(earliest_observation_start_date) &
    !is.null(latest_observation_start_date)) {
    checkmate::assertTRUE(latest_observation_start_date >= earliest_observation_start_date)
  }
  if (!is.null(min_days_to_observation_end) &
    !is.null(max_days_to_observation_end)) {
    checkmate::assertTRUE(max_days_to_observation_end >= min_days_to_observation_end)
  }
  if (!is.null(earliest_condition_start_date) &
    !is.null(latest_condition_start_date)) {
    checkmate::assertTRUE(latest_condition_start_date >= earliest_condition_start_date)
  }
  if (!is.null(min_days_to_condition_end) &
    !is.null(max_days_to_condition_end)) {
    checkmate::assertTRUE(max_days_to_condition_end >= min_days_to_condition_end)
  }
  if (!is.null(earliest_visit_start_date) &
    !is.null(latest_visit_start_date)) {
    checkmate::assertTRUE(latest_visit_start_date >= earliest_visit_start_date)
  }
  if (!is.null(min_days_to_visit_end) &
    !is.null(max_days_to_visit_end)) {
    checkmate::assertTRUE(max_days_to_visit_end >= min_days_to_visit_end)
  }
  if (length(listTables) > 1) {
    checkmate::assertTRUE(length(listTables) == length(names(listTables)))
  }
  if (length(listTables) > 1) {
    for (i in length(listTables) > 1) {
      checkmate::assert_tibble(listTables[[i]], null.ok = TRUE)
    }
  }
  checkmate::reportAssertions(collection = errorMessage)



  set.seed(seed) # set seeds

  # create drug strength table
  if (is.null(drug_strength)) {
    drug_concept_id <-
      seq(1:drug_concept_id_size) # create unique drug concept id
    ingredient_concept_id <-
      seq(1:ingredient_concept_id_size) # create ingredient concept id
    amount_value <-
      c(
        rep(NA, each = ingredient_concept_id_size),
        # ingredient have missing amount value
        sample(c("10", "20", "30"),
          drug_concept_id_size - 1,
          replace = TRUE
        )
      ) # compute amount value
    amount_unit_concept_id <-
      sample(c("8576"),
        drug_concept_id_size,
        replace = TRUE
      ) #  compute unit id


    drug_strength <-
      data.frame(
        drug_concept_id = as.numeric(drug_concept_id),
        ingredient_concept_id = as.numeric(
          sample(ingredient_concept_id, drug_concept_id_size, replace = TRUE)
        ),
        amount_value = as.numeric(amount_value),
        amount_unit_concept_id = as.numeric(amount_unit_concept_id)
        # numerator_value = numeric(),
        # numerator_unit_concept_id = numeric(),
        # denominator_value = numeric(),
        # denominator_unit_concept_id = numeric(),
        # box_size = numeric(),
        # valid_start_date = as.Date(character()),
        # valid_end_date = as.Date(character()),
        # invalid_reason = character()
      )
  }

  # create drug strength table
  if (is.null(drug_era)) {
    drug_concept_id <-
      seq(1:drug_concept_id_size) # create unique drug concept id
    person_id <-
      rep(1, drug_concept_id_size) # create ingredient concept id
    drug_era_start_date <- rep(as.Date("2000-01-01"), drug_concept_id_size)
    drug_era_end_date <- rep(as.Date("2010-01-01"), drug_concept_id_size)
    drug_era <-
      data.frame(
        drug_concept_id = drug_concept_id,
        person_id = person_id,
        drug_era_start_date = drug_era_start_date,
        drug_era_end_date = drug_era_end_date
      )
  }


  if (is.null(condition_era)) {
    condition_concept_id <-
      seq(1:condition_concept_id_size) # create unique condition concept id
    person_id <-
      rep(1, condition_concept_id_size) # create ingredient concept id
    condition_era_start_date <- rep(as.Date("2000-01-01"), condition_concept_id_size)
    condition_era_end_date <- rep(as.Date("2010-01-01"), condition_concept_id_size)
    condition_era <-
      data.frame(
        condition_concept_id = condition_concept_id,
        person_id = person_id,
        condition_era_start_date = condition_era_start_date,
        condition_era_end_date = condition_era_end_date
      )
  }



  if (is.null(specimen)) {
    specimen_concept_id <-
      seq(1:specimen_concept_id_size) # create unique condition concept id
    person_id <-
      rep(1, condition_concept_id_size) # create ingredient concept id
    specimen_date <- rep(as.Date("2000-01-01"), condition_concept_id_size)
    specimen <-
      data.frame(
        specimen_concept_id = specimen_concept_id,
        person_id = person_id,
        specimen_date = specimen_date
      )
  }

  # drug_exposure
  if (is.null(drug_exposure)) {
    drug_exposure_id <-
      as.integer(seq(1:drug_exposure_size)) # generate number of unique drug_exposure_id
    person_id <-
      as.integer(sample(seq(1:patient_size),
        drug_exposure_size,
        replace = TRUE
      )) # generate number of unique patient id
    drug_concept_id <-
      as.integer(sample(
        drug_strength$drug_concept_id,
        drug_exposure_size,
        replace = TRUE
      )) # assign drug concept id to to each drug exposure

    # generate drug exposure start date
    drug_exposure_start_date <-
      sample(
        seq(
          as.Date(min_drug_exposure_start_date),
          as.Date(max_drug_exposure_start_date),
          by = "day"
        ),
        drug_exposure_size,
        replace = TRUE
      )
    # generate drug exposure end date to happens after drug exposure start date
    drug_exposure_end_date <-
      drug_exposure_start_date + lubridate::days(sample(c(0, 7, 14, 21, 28, 30, 60, 90),
        drug_exposure_size,
        replace = TRUE
      ))

    days_supply <-
      as.integer(difftime(drug_exposure_end_date, drug_exposure_start_date, units = "days"))

    quantity <- days_supply + 1



    # putting into drug_exposure table
    drug_exposure <-
      data.frame(
        drug_exposure_id = as.numeric(drug_exposure_id),
        person_id = as.numeric(person_id),
        drug_concept_id = as.numeric(drug_concept_id),
        drug_exposure_start_date = drug_exposure_start_date,
        drug_exposure_end_date = drug_exposure_end_date,
        quantity = as.numeric(quantity)
        ##  days_supply = as.numeric(days_supply)
      )
  }
  # person table
  id <- sample(seq(1:patient_size))
  # person gender
  gender_id <- sample(c("8507", "8532"),
    patient_size,
    replace = TRUE
  )

  if (is.null(person) | is.null(observation_period)) {
    # Define earliest possible date of birth for person table
    if (is.null(earliest_date_of_birth)) {
      earliest_date_of_birth <- as.Date("1920-01-01")
    }
    # Define latest possible date of birth for person table
    if (is.null(latest_date_of_birth)) {
      latest_date_of_birth <- as.Date("2000-01-01")
    }

    DOB <- sample(
      seq(
        as.Date(earliest_date_of_birth),
        as.Date(latest_date_of_birth),
        by = "day"
      ),
      patient_size,
      replace = TRUE
    )
    # year, month, day
    DOB_year <- as.numeric(format(DOB, "%Y"))
    DOB_month <- as.numeric(format(DOB, "%m"))
    DOB_day <- as.numeric(format(DOB, "%d"))

    # observation_period table
    # create a list of observational_period_id

    # define earliest and latest observation start date for obs table
    # if not specified by user
    if (is.null(earliest_observation_start_date)) {
      earliest_observation_start_date <- as.Date("2005-01-01")
    }
    if (is.null(latest_observation_start_date)) {
      latest_observation_start_date <- as.Date("2010-01-01")
    }
    obs_start_date <-
      sample(
        seq(
          as.Date(earliest_observation_start_date),
          as.Date(latest_observation_start_date),
          by = "day"
        ),
        patient_size,
        replace = TRUE
      ) # start date for the period


    # define min and max day to condition end
    if (is.null(min_days_to_observation_end)) {
      min_days_to_observation_end <- 5000
    }
    if (is.null(max_days_to_observation_end)) {
      max_days_to_observation_end <- 5000
    }

    obs_end_date <-
      obs_start_date + lubridate::days(
        sample(
          min_days_to_observation_end:max_days_to_observation_end,
          patient_size,
          replace = TRUE
        )
      )
  }



  if (is.null(person) | is.null(condition_occurrence)) {
    # define earliest and latest condition start date for obs table
    # if not specified by user
    if (is.null(earliest_condition_start_date)) {
      earliest_condition_start_date <- as.Date("2005-01-01")
    }
    if (is.null(latest_condition_start_date)) {
      latest_condition_start_date <- as.Date("2020-01-01")
    }
    condition_start_date <-
      sample(
        seq(
          as.Date(earliest_condition_start_date),
          as.Date(latest_condition_start_date),
          by = "day"
        ),
        patient_size,
        replace = TRUE
      ) # start date for the period


    # define min and max day to condition end
    if (is.null(min_days_to_condition_end)) {
      min_days_to_condition_end <- 1
    }
    if (is.null(max_days_to_condition_end)) {
      max_days_to_condition_end <- 1000
    }

    condition_end_date <-
      condition_start_date + lubridate::days(
        sample(
          min_days_to_condition_end:max_days_to_condition_end,
          patient_size,
          replace = TRUE
        )
      )

    c_concept_id <-
      seq(1:condition_concept_id_size)
    condition_concept_id <- sample(c_concept_id,
      patient_size,
      replace = TRUE
    )
  }


  if (is.null(person) | is.null(visit_occurrence)) {
    # define earliest and latest visit start date for obs table
    # if not specified by user
    if (is.null(earliest_visit_start_date)) {
      earliest_visit_start_date <- as.Date("2005-01-01")
    }
    if (is.null(latest_visit_start_date)) {
      latest_visit_start_date <- as.Date("2020-01-01")
    }
    visit_start_date <-
      sample(
        seq(
          as.Date(earliest_visit_start_date),
          as.Date(latest_visit_start_date),
          by = "day"
        ),
        patient_size,
        replace = TRUE
      ) # start date for the period


    # define min and max day to visit end
    if (is.null(min_days_to_visit_end)) {
      min_days_to_visit_end <- 1
    }
    if (is.null(max_days_to_visit_end)) {
      max_days_to_visit_end <- 1000
    }

    visit_end_date <-
      visit_start_date + lubridate::days(
        sample(
          min_days_to_visit_end:max_days_to_visit_end,
          patient_size,
          replace = TRUE
        )
      )

    v_concept_id <- seq(1:visit_concept_id_size)

    visit_concept_id <- sample(v_concept_id,
      patient_size,
      replace = TRUE
    )

    v_occurrence_id <- seq(1:visit_occurrence_id_size)

    visit_occurrence_id <- sample(v_occurrence_id,
      patient_size,
      replace = TRUE
    )
  }

  if (is.null(person)) {
    person <- dplyr::tibble(
      person_id = id,
      gender_concept_id = gender_id,
      year_of_birth = DOB_year,
      month_of_birth = DOB_month,
      day_of_birth = DOB_day
    )
  }

  if (is.null(observation_period)) {
    observation_period <- dplyr::tibble(
      observation_period_id = id,
      person_id = id,
      observation_period_start_date = obs_start_date,
      observation_period_end_date = obs_end_date
    )
  }


  if (is.null(observation)) {
    observation <- dplyr::tibble(
      observation_concept_id = id,
      person_id = id,
      observation_date = obs_start_date
    )
  }
  if (is.null(condition_occurrence)) {
    condition_occurrence <- dplyr::tibble(
      condition_occurrence_id = id,
      person_id = id,
      condition_concept_id = condition_concept_id,
      condition_start_date = condition_start_date,
      condition_end_date = condition_end_date
    )
  }


  if (is.null(visit_occurrence)) {
    id <- sample(seq(1:patient_size))

    visit_occurrence <- dplyr::tibble(
      visit_occurrence_id = visit_occurrence_id,
      person_id = id,
      visit_concept_id = visit_concept_id,
      visit_start_date = visit_start_date,
      visit_end_date = visit_end_date
    )
  }

  if (is.null(concept_ancestor)) {
    ancestor_concept_id <-
      seq(1:ancestor_concept_id_size)
    descendant_concept_id <-
      seq((ancestor_concept_id_size + 1):(ancestor_concept_id_size + ancestor_concept_id_size))
    concept_ancestor <- data.frame(
      ancestor_concept_id = as.numeric(ancestor_concept_id),
      descendant_concept_id = as.numeric(descendant_concept_id)
    )
  }

  if (is.null(concept)) {
    concept_id_values <-
      seq(1:concept_id_size)
    concept_name_values <-
      paste0("concept_name_", 1:concept_id_size)
    concept <- data.frame(
      concept_id = as.numeric(concept_id_values),
      concept_name = c(concept_name_values)
    )
  }


  if (is.null(measurement)) {
    person_id <-
      seq(1:measurement_size)
    measurement_concept_id <-
      seq(1:measurement_size)
    measurement_date <- c(rep(as.Date("2010-01-01"), measurement_size))
    measurement <- data.frame(
      person_id = person_id,
      measurement_concept_id = c(measurement_concept_id),
      measurement_date = measurement_date
    )
  }

  if (is.null(procedure_occurrence)) {
    person_id <-
      seq(1:procedure_occurrence_size)
    procedure_concept_id <-
      seq(1:procedure_occurrence_size)
    procedure_date <- c(rep(as.Date("2009-01-01"), procedure_occurrence_size))

    procedure_occurrence <- data.frame(
      person_id = person_id,
      procedure_concept_id = procedure_concept_id,
      procedure_date = procedure_date
    )
  }

  if (is.null(device_exposure)) {
    person_id <-
      seq(1:device_exposure_size)
    device_concept_id <-
      seq(1:device_exposure_size)
    device_exposure_start_date <- c(rep(as.Date("2009-01-01"), device_exposure_size))
    device_exposure_end_date <- c(rep(as.Date("2011-01-01"), device_exposure_size))

    device_exposure <- data.frame(
      person_id = person_id,
      device_concept_id = device_concept_id,
      device_exposure_start_date = device_exposure_start_date,
      device_exposure_end_date = device_exposure_end_date
    )
  }


  # cohort table 1
  if (is.null(cohort1)) {
    cohort1 <- dplyr::tibble(
      cohort_definition_id = c(1, 1, 1, 2),
      subject_id = c(1, 1, 2, 3),
      cohort_start_date = as.Date(c("2020-01-01", "2020-06-01", "2020-01-02", "2020-01-01")),
      cohort_end_date = as.Date(c("2020-04-01", "2020-08-01", "2020-02-02", "2020-03-01"))
    )
  }
  # cohort table 2
  if (is.null(cohort2)) {
    cohort2 <- dplyr::tibble(
      cohort_definition_id = c(1, 1, 2, 3, 1),
      subject_id = c(1, 3, 1, 2, 1),
      cohort_start_date = as.Date(c("2019-12-30", "2020-01-01", "2020-05-25", "2020-01-01", "2020-05-25")),
      cohort_end_date = as.Date(c("2019-12-30", "2020-01-01", "2020-05-25", "2020-01-01", "2020-05-25"))
    )
  }




  # into in-memory database
  db <- DBI::dbConnect(duckdb::duckdb(), ":memory:")


  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "drug_strength",
      drug_strength,
      overwrite = TRUE
    )
  })

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "drug_exposure",
      drug_exposure,
      overwrite = TRUE
    )
  })

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "person",
      person,
      overwrite = TRUE
    )
  })

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "observation",
      observation,
      overwrite = TRUE
    )
  })

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "observation_period",
      observation_period,
      overwrite = TRUE
    )
  })

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "condition_occurrence",
      condition_occurrence,
      overwrite = TRUE
    )
  })

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "measurement",
      measurement,
      overwrite = TRUE
    )
  })

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "procedure_occurrence",
      procedure_occurrence,
      overwrite = TRUE
    )
  })

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "device_exposure",
      device_exposure,
      overwrite = TRUE
    )
  })

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "drug_era",
      drug_era,
      overwrite = TRUE
    )
  })

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "visit_occurrence",
      visit_occurrence,
      overwrite = TRUE
    )
  })

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "concept_ancestor",
      concept_ancestor,
      overwrite = TRUE
    )
  })


  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "concept",
      concept,
      overwrite = TRUE
    )
  })



  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "condition_era",
      condition_era,
      overwrite = TRUE
    )
  })


  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "specimen",
      specimen,
      overwrite = TRUE
    )
  })


  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "cohort1",
      cohort1,
      overwrite = TRUE
    )
  })

  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "cohort2",
      cohort2,
      overwrite = TRUE
    )
  })
  if (length(listTables) > 0) {
    for (i in 1:length(listTables)) {
      DBI::dbWithTransaction(db, {
        DBI::dbWriteTable(db, names(listTables)[i],
          listTables[[i]],
          overwrite = TRUE
        )
      })
    }
  }
  if (length(listTables) > 0) {
    cdm <- CDMConnector::cdm_from_con(
      db,
      cdm_tables = c(
        "drug_strength",
        "drug_exposure",
        "person",
        "concept_ancestor",
        "concept",
        "observation_period",
        "condition_occurrence",
        "visit_occurrence",
        "measurement",
        "drug_era",
        "procedure_occurrence",
        "condition_era",
        "specimen",
        "device_exposure",
        "observation"
      ),
      write_schema = "main",
      cohort_tables = c("cohort1", "cohort2", names(listTables)),
      cdm_name = "mock"
    )
  } else {
    cdm <- CDMConnector::cdm_from_con(
      db,
      cdm_tables = c(
        "drug_strength",
        "drug_exposure",
        "person",
        "concept_ancestor",
        "concept",
        "observation_period",
        "condition_occurrence",
        "visit_occurrence",
        "measurement",
        "drug_era",
        "procedure_occurrence",
        "condition_era",
        "specimen",
        "device_exposure",
        "observation"
      ),
      write_schema = "main",
      cohort_tables = c("cohort1", "cohort2"),
      cdm_name = "mock"
    )
  }

  return(cdm)
}
