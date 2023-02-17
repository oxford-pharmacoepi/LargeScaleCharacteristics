test_that("check addLargeScaleCharacteristics overlap works", {
  cohort1 <- tibble::tibble(
    cohort_definition_id = c("1"),
    subject_id = c("1"),
    cohort_start_date = c(
      as.Date("2010-04-01")
    ),
    cohort_end_date = c(
      as.Date("2011-06-01")
    )
  )

  drug_era <- tibble::tibble(
    person_id = c("1"),
    drug_era_start_date = c(as.Date("2010-03-03")),
    drug_era_end_date = c(as.Date("2010-05-03")),
    drug_concept_id = c("1")
  )

  cdm <- mockLargeScaleCharacteristics(
    cohort1 = cohort1,
    drug_era = drug_era
  )


  result_allow_overlap <- addLargeScaleCharacteristics(
    x = cdm$cohort1,
    cdm,
    overlap = TRUE,
    temporalWindows = list(c(-30, -1), c(1, 30)),
    tablesToCharacterize = c("drug_era")
  ) %>% dplyr::collect()

  result_no_overlap <- addLargeScaleCharacteristics(
    x = cdm$cohort1,
    cdm,
    overlap = FALSE,
    temporalWindows = list(c(-30, -1), c(1, 30)),
    tablesToCharacterize = c("drug_era")
  ) %>% dplyr::collect()

  expect_true(result_allow_overlap$`drug_era_1_1;30` == 1)
  expect_true(result_no_overlap$`drug_era_1_1;30` == 0)
})


test_that("check more examples", {
  cohort1 <- tibble::tibble(
    cohort_definition_id = c("1"),
    subject_id = c("1"),
    cohort_start_date = c(
      as.Date("2010-04-01")
    ),
    cohort_end_date = c(
      as.Date("2011-06-01")
    )
  )
  device_exposure <- tibble::tibble(
    person_id = c("1"),
    device_exposure_start_date = c(as.Date("2009-05-01")),
    device_exposure_end_date = c(as.Date("2010-03-03")),
    device_concept_id = c("1")
  )

  cdm <- mockLargeScaleCharacteristics(
    cohort1 = cohort1,
    device_exposure = device_exposure
  )


  check_expected_windows <- addLargeScaleCharacteristics(
    x = cdm$cohort1,
    cdm,
    overlap = TRUE,
    temporalWindows = list(c(-365, -91),  c(-90, -1), c(-30, -1), c(NA, -366),
                           c(0, 0), c(1, 30), c(1, 90)),
    tablesToCharacterize = c("device_exposure")
  ) %>% dplyr::collect()


  expect_true(all(check_expected_windows[,4:10] == c(1,1,1,0,0,0,0)))
})



test_that("check each supported table works", {
  cohort1 <- tibble::tibble(
    cohort_definition_id = c("1"),
    subject_id = c("1"),
    cohort_start_date = c(
      as.Date("2010-04-01")
    ),
    cohort_end_date = c(
      as.Date("2011-06-01")
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

  expect_no_error(addLargeScaleCharacteristics(
    x = cdm$cohort1,
    cdm,
    tablesToCharacterize = "visit_occurrence"
  ))
  expect_no_error(addLargeScaleCharacteristics(
    x = cdm$cohort1,
    cdm,
    tablesToCharacterize = "condition_occurrence"
  ))
  expect_no_error(addLargeScaleCharacteristics(
    x = cdm$cohort1,
    cdm,
    tablesToCharacterize = "drug_exposure"
  ))
  expect_no_error(addLargeScaleCharacteristics(
    x = cdm$cohort1,
    cdm,
    tablesToCharacterize = "procedure_occurrence"
  ))
  expect_no_error(addLargeScaleCharacteristics(
    x = cdm$cohort1,
    cdm,
    tablesToCharacterize = "device_exposure"
  ))
  expect_no_error(addLargeScaleCharacteristics(
    x = cdm$cohort1,
    cdm,
    tablesToCharacterize = "measurement"
  ))
  expect_no_error(addLargeScaleCharacteristics(
    x = cdm$cohort1,
    cdm,
    tablesToCharacterize = "observation"
  ))
  expect_no_error(addLargeScaleCharacteristics(
    x = cdm$cohort1,
    cdm,
    tablesToCharacterize = "drug_era"
  ))
  expect_no_error(addLargeScaleCharacteristics(
    x = cdm$cohort1,
    cdm,
    tablesToCharacterize = "condition_era"
  ))
  expect_no_error(addLargeScaleCharacteristics(
    x = cdm$cohort1,
    cdm,
    tablesToCharacterize = "specimen"))
})



test_that("check if missing cohort start date in x, throw error", {
  cohort1 <- tibble::tibble(
    cohort_definition_id = c("1"),
    subject_id = c("1"),
    cohort_start_date = c(
      as.Date(NA)
    ),
    cohort_end_date = c(
      as.Date("2011-06-01")
    )
  )

  cdm <- mockLargeScaleCharacteristics(
    cohort1 = cohort1  )


  expect_error(addLargeScaleCharacteristics(
    x = cdm$cohort1,
    cdm))

  cohort1 <- tibble::tibble(
    cohort_definition_id = c("1"),
    subject_id = c("1"),
    cohort_start_date = c(
      as.Date("2011-06-01")
    ),
    cohort_end_date = c(
      as.Date(NA)
    )
  )

  cdm <- mockLargeScaleCharacteristics(
    cohort1 = cohort1  )


  expect_error(addLargeScaleCharacteristics(
    x = cdm$cohort1,
    cdm))

  cohort1 <- tibble::tibble(
    cohort_definition_id = NA,
    subject_id = c("1"),
    cohort_start_date = c(
      as.Date("2011-06-01")
    ),
    cohort_end_date = c(
      as.Date("2011-06-01")
    )
  )

  cdm <- mockLargeScaleCharacteristics(
    cohort1 = cohort1  )


  expect_error(addLargeScaleCharacteristics(
    x = cdm$cohort1,
    cdm))

  cohort1 <- tibble::tibble(
    cohort_definition_id = "1",
    subject_id = NA,
    cohort_start_date = c(
      as.Date("2011-06-01")
    ),
    cohort_end_date = c(
      as.Date("2011-06-01")
    )
  )

  cdm <- mockLargeScaleCharacteristics(
    cohort1 = cohort1  )


  expect_error(addLargeScaleCharacteristics(
    x = cdm$cohort1,
    cdm))
})

