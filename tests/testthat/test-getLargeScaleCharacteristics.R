test_that("check getLargeScaleCharacteristics inputs checks", {
  cohort1 <- tibble::tibble(
    cohort_definition_id = c("1"),
    subject_id = c("1"),
    cohort_start_date = c(
      as.Date("2010-03-03")
    ),
    cohort_end_date = c(
      as.Date("2012-01-01")
    )
  )
  cohort2 <- tibble::tibble(
    cohort_definition_id = c("1", "1", "1"),
    subject_id = c("1", "2", "3"),
    cohort_start_date = c(
      as.Date("2000-03-03"), as.Date("2000-03-01"), as.Date("2000-02-01")
    ),
    cohort_end_date = c(
      as.Date("2020-01-01"), as.Date("2020-01-01"), as.Date("2019-01-01")
    )
  )

  drug_era <- tibble::tibble(
    person_id = c("1"),
    drug_era_start_date = c(as.Date("2000-03-03")),
    drug_era_end_date = c(as.Date("2010-03-03")),
    drug_concept_id = c("1")
  )



  cdm <- mockLargeScaleCharacteristics(
    cohort1 = cohort1, cohort2 = cohort2,
    drug_era = drug_era
  )

  # throw error if targetCohortName more than 1, does not allow list
  expect_error(getLargeScaleCharacteristics(cdm,
    targetCohortName = c("cohort1", "cohort2"),
    tablesToCharacterize = c("drug_exposure")
  ))
  expect_error(getLargeScaleCharacteristics(cdm,
    targetCohortName = list("cohort1"),
    tablesToCharacterize = c("drug_exposure")
  ))

  # throw error if temporalWindows not in a list
  expect_error(
    getLargeScaleCharacteristics(cdm,
      targetCohortName = c("cohort1"),
      temporalWindows = c(NA, -366),
      tablesToCharacterize = c("drug_exposure")
    )
  )
  expect_no_error(
    getLargeScaleCharacteristics(cdm,
      targetCohortName = c("cohort1"),
      temporalWindows = list(c(NA, -366)),
      tablesToCharacterize = c("drug_exposure", "condition_occurrence")
    )
  )

  # throw error if overlap length and tablesToCharacterize length differ
  expect_error(
    getLargeScaleCharacteristics(cdm,
      targetCohortName = c("cohort1"),
      temporalWindows = list(c(NA, -366)),
      tablesToCharacterize = c("drug_exposure"),
      overlap = c(TRUE, TRUE)
    )
  )
  expect_no_error(
    getLargeScaleCharacteristics(cdm,
      targetCohortName = c("cohort1"),
      temporalWindows = list(c(NA, -366)),
      tablesToCharacterize = c("drug_exposure", "condition_occurrence"),
      overlap = c(TRUE, TRUE)
    )
  )

  # throw error if targetCohortId is character not integer, should allow vector
  expect_error(
    getLargeScaleCharacteristics(cdm,
      targetCohortName = c("cohort1"),
      targetCohortId = "1",
      temporalWindows = list(c(NA, -366)),
      tablesToCharacterize = c("drug_exposure"),
      overlap = TRUE
    )
  )
  expect_no_error(
    getLargeScaleCharacteristics(cdm,
      targetCohortName = c("cohort1"),
      targetCohortId = c(1, 2),
      temporalWindows = list(c(NA, -366)),
      tablesToCharacterize = c("drug_exposure"),
      overlap = TRUE
    )
  )

  cdm$device_exposure <- NULL
  expect_error(getLargeScaleCharacteristics(cdm,
    targetCohortName = c("cohort1"),
    tablesToCharacterize = c("device_exposure")
  ))
})




test_that("check overlap and drug era table works", {
  cohort1 <- tibble::tibble(
    cohort_definition_id = c(rep("1", 9)),
    subject_id = c(rep("1", 9)),
    cohort_start_date = c(
      as.Date("2012-03-01"),
      as.Date("2012-03-02"),
      as.Date("2012-03-03"),
      as.Date("2012-03-04"),
      as.Date("2012-03-05"),
      as.Date("2012-03-06"),
      as.Date("2012-04-01"),
      as.Date("2012-05-01"),
      as.Date("2012-06-01")
    ),
    cohort_end_date = c(
      as.Date("2016-01-01"),
      as.Date("2016-01-01"),
      as.Date("2016-01-01"),
      as.Date("2016-01-01"),
      as.Date("2016-01-01"),
      as.Date("2016-01-01"),
      as.Date("2016-01-01"),
      as.Date("2017-01-01"),
      as.Date("2019-01-01")
    )
  )
  observation_period <- tibble::tibble(
    person_id = c("1"),
    observation_period_start_date = c(as.Date("2000-01-01")),
    observation_period_end_date = c(as.Date("2020-03-03"))
  )

  drug_era <- tibble::tibble(
    person_id = c("1", "1"),
    drug_era_start_date = c(as.Date("2011-02-01")),
    drug_era_end_date = c(as.Date("2011-05-01")),
    drug_concept_id = c("1", "1")
  )

  cdm <- mockLargeScaleCharacteristics(
    cohort1 = cohort1,
    drug_era = drug_era,
    observation_period = observation_period
  )

  result_allow_overlap <- getLargeScaleCharacteristics(cdm,
    targetCohortName = c("cohort1"),
    tablesToCharacterize = c("drug_era"),
    overlap = TRUE
  )

  expect_true(result_allow_overlap[result_allow_overlap$window_name == "-365;-31", ]$concept_count == 7)
  expect_true(result_allow_overlap[result_allow_overlap$window_name == "-365;-91", ]$concept_count == 7)

  result_no_overlap <- getLargeScaleCharacteristics(cdm,
    targetCohortName = c("cohort1"),
    tablesToCharacterize = c("drug_era"),
    overlap = FALSE
  )
  expect_true(any(result_no_overlap$window_name == "-365;-31") == FALSE)

  # test another cohort
  cohort1 <- tibble::tibble(
    cohort_definition_id = c(rep("1", 9)),
    subject_id = c(rep("1", 9)),
    cohort_start_date = c(
      as.Date("2010-03-01"),
      as.Date("2010-03-02"),
      as.Date("2010-03-03"),
      as.Date("2010-03-04"),
      as.Date("2010-03-05"),
      as.Date("2010-03-06"),
      as.Date("2010-04-01"),
      as.Date("2010-05-01"),
      as.Date("2010-06-01")
    ),
    cohort_end_date = c(
      as.Date("2016-01-01"),
      as.Date("2016-01-01"),
      as.Date("2016-01-01"),
      as.Date("2016-01-01"),
      as.Date("2016-01-01"),
      as.Date("2016-01-01"),
      as.Date("2016-01-01"),
      as.Date("2017-01-01"),
      as.Date("2019-01-01")
    )
  )

  cdm <- mockLargeScaleCharacteristics(
    cohort1 = cohort1,
    drug_era = drug_era,
    observation_period = observation_period
  )

  result_allow_overlap <- getLargeScaleCharacteristics(cdm,
    targetCohortName = c("cohort1"),
    tablesToCharacterize = c("drug_era"),
    overlap = TRUE
  )
  expect_true(result_allow_overlap[result_allow_overlap$window_name == "366;Any", ]$concept_count == 7)
  expect_true(result_allow_overlap[result_allow_overlap$window_name == "31;365", ]$concept_count == 9)
  expect_true(result_allow_overlap[result_allow_overlap$window_name == "91;365", ]$concept_count == 9)

  result_no_overlap <- getLargeScaleCharacteristics(cdm,
    targetCohortName = c("cohort1"),
    tablesToCharacterize = c("drug_era"),
    overlap = FALSE
  )
  expect_true(any(result_no_overlap$window_name == "366;Any") == FALSE)
})





test_that("check each supported table works", {
  cohort1 <- tibble::tibble(
    cohort_definition_id = c(rep("1", 9)),
    subject_id = c(rep("1", 9)),
    cohort_start_date = c(
      as.Date("2012-03-01"),
      as.Date("2012-03-02"),
      as.Date("2012-03-03"),
      as.Date("2012-03-04"),
      as.Date("2012-03-05"),
      as.Date("2012-03-06"),
      as.Date("2012-04-01"),
      as.Date("2012-05-01"),
      as.Date("2012-06-01")
    ),
    cohort_end_date = c(
      as.Date("2016-01-01"),
      as.Date("2016-01-01"),
      as.Date("2016-01-01"),
      as.Date("2016-01-01"),
      as.Date("2016-01-01"),
      as.Date("2016-01-01"),
      as.Date("2016-01-01"),
      as.Date("2017-01-01"),
      as.Date("2019-01-01")
    )
  )
  observation_period <- tibble::tibble(
    person_id = c("1"),
    observation_period_start_date = c(as.Date("2000-01-01")),
    observation_period_end_date = c(as.Date("2020-03-03"))
  )

  procedure_occurrence <- tibble::tibble(
    person_id = c("1"),
    procedure_date = c(as.Date("2010-03-03")),
    procedure_concept_id = c("1")
  )

  device_exposure <- tibble::tibble(
    person_id = c("1"),
    device_exposure_start_date = c(as.Date("2009-01-01")),
    device_exposure_end_date = c(as.Date("2010-03-03")),
    device_concept_id = c("1")
  )

  condition_occurrence <- tibble::tibble(
    person_id = c("1"),
    condition_start_date = c(as.Date("2008-01-01")),
    condition_end_date = c(as.Date("2010-03-03")),
    condition_concept_id = c("1")
  )

  condition_era <- tibble::tibble(
    person_id = c("1"),
    condition_era_start_date = c(as.Date("2008-01-01")),
    condition_era_end_date = c(as.Date("2010-03-03")),
    condition_concept_id = c("1")
  )

  observation <- tibble::tibble(
    person_id = c("1"),
    observation_date = c(as.Date("2008-01-01")),
    observation_concept_id = c("1")
  )


  visit_occurrence <- tibble::tibble(
    person_id = c("1"),
    visit_start_date = c(as.Date("2008-01-01")),
    visit_end_date = c(as.Date("2010-03-03")),
    visit_concept_id = c("1")
  )

  drug_era <- tibble::tibble(
    person_id = c("1"),
    drug_era_start_date = c(as.Date("2011-02-01")),
    drug_era_end_date = c(as.Date("2011-05-01")),
    drug_concept_id = c("1")
  )

  drug_exposure <- tibble::tibble(
    person_id = c("1"),
    drug_exposure_start_date = c(as.Date("2011-02-01")),
    drug_exposure_end_date = c(as.Date("2011-05-01")),
    drug_concept_id = c("1")
  )

  measurement <- tibble::tibble(
    person_id = c("1"),
    measurement_date = c(as.Date("2010-04-03")),
    measurement_concept_id = c("3")
  )

  specimen <- tibble::tibble(
    person_id = c("1"),
    specimen_date = c(as.Date("2010-04-03")),
    specimen_concept_id = c("3")
  )

  drug_strength <- dplyr::tibble(
    drug_concept_id = c(1, 2, 3, 4, 5),
    ingredient_concept_id = c(1, 1, 1, 1, 1),
    amount_value = c(100, NA, NA, NA, NA)
  )

  cdm <- mockLargeScaleCharacteristics(
    cohort1 = cohort1,
    procedure_occurrence = procedure_occurrence,
    measurement = measurement,
    drug_exposure = drug_exposure,
    drug_era = drug_era,
    specimen = specimen,
    visit_occurrence = visit_occurrence,
    condition_era = condition_era,
    condition_occurrence = condition_occurrence,
    device_exposure = device_exposure,
    observation_period = observation_period,
    observation = observation,
    drug_strength = drug_strength
  )

  # check all supported tables separately should not throw any error
  expect_no_error(getLargeScaleCharacteristics(cdm,
    targetCohortName = c("cohort1"),
    overlap = TRUE,
    tablesToCharacterize = "visit_occurrence"
  ))
  expect_no_error(getLargeScaleCharacteristics(cdm,
    targetCohortName = c("cohort1"),
    overlap = TRUE,
    tablesToCharacterize = "condition_occurrence"
  ))
  expect_no_error(getLargeScaleCharacteristics(cdm,
    targetCohortName = c("cohort1"),
    overlap = TRUE,
    tablesToCharacterize = "drug_exposure"
  ))
  expect_no_error(getLargeScaleCharacteristics(cdm,
    targetCohortName = c("cohort1"),
    overlap = TRUE,
    tablesToCharacterize = "procedure_occurrence"
  ))
  expect_no_error(getLargeScaleCharacteristics(cdm,
    targetCohortName = c("cohort1"),
    overlap = TRUE,
    tablesToCharacterize = "device_exposure"
  ))
  expect_no_error(getLargeScaleCharacteristics(cdm,
    targetCohortName = c("cohort1"),
    overlap = TRUE,
    tablesToCharacterize = "measurement"
  ))
  expect_no_error(getLargeScaleCharacteristics(cdm,
    targetCohortName = c("cohort1"),
    overlap = TRUE,
    tablesToCharacterize = "observation"
  ))
  expect_no_error(getLargeScaleCharacteristics(cdm,
    targetCohortName = c("cohort1"),
    overlap = TRUE,
    tablesToCharacterize = "drug_era"
  ))
  expect_no_error(getLargeScaleCharacteristics(cdm,
    targetCohortName = c("cohort1"),
    overlap = TRUE,
    tablesToCharacterize = "condition_era"
  ))
  expect_no_error(getLargeScaleCharacteristics(cdm,
    targetCohortName = c("cohort1"),
    overlap = TRUE,
    tablesToCharacterize = "specimen"
  ))

  # throw error if input not supported table
  expect_error(getLargeScaleCharacteristics(cdm,
    targetCohortName = c("cohort1"),
    overlap = TRUE,
    tablesToCharacterize = "drug_strength"
  ))
})





test_that("check multiple target cohort IDs works", {
  cohort1 <- tibble::tibble(
    cohort_definition_id = c("1", "2"),
    subject_id = c("1", "1"),
    cohort_start_date = c(
      as.Date("2010-03-03"),
      as.Date("2010-01-03")
    ),
    cohort_end_date = c(
      as.Date("2012-01-01"),
      as.Date("2011-03-01")
    )
  )
  cohort2 <- tibble::tibble(
    cohort_definition_id = c("1", "1", "1"),
    subject_id = c("1", "2", "3"),
    cohort_start_date = c(
      as.Date("2000-03-03"), as.Date("2000-03-01"), as.Date("2000-02-01")
    ),
    cohort_end_date = c(
      as.Date("2020-01-01"), as.Date("2020-01-01"), as.Date("2019-01-01")
    )
  )

  drug_era <- tibble::tibble(
    person_id = c("1"),
    drug_era_start_date = c(as.Date("2000-03-03")),
    drug_era_end_date = c(as.Date("2010-03-03")),
    drug_concept_id = c("1")
  )

  cdm <- mockLargeScaleCharacteristics(
    cohort1 = cohort1, cohort2 = cohort2,
    drug_era = drug_era
  )

  result <- getLargeScaleCharacteristics(cdm,
    targetCohortName = c("cohort1"),
    overlap = TRUE,
    tablesToCharacterize = "drug_era",
    temporalWindows = list(c(1, 30))
  )

  expect_true(unique(result$cohort_definition_id) == 2)
})
