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


  result_allow_overlap <- addLargeScaleCharacteristics(cdm,
                                         targetCohortName = c("cohort1"),
                                         overlap = TRUE,
                                         temporalWindows = list( c(-30, -1), c(1, 30)),
                                         tablesToCharacterize = c("drug_era"))

  result_no_overlap <- addLargeScaleCharacteristics(cdm,
                                         targetCohortName = c("cohort1"),
                                         overlap = FALSE,
                                         temporalWindows = list( c(-30, -1), c(1, 30)),
                                         tablesToCharacterize = c("drug_era"))

  expect_true(result_allow_overlap$`drug_era_1_1;30` == 1)
  expect_true(result_no_overlap$`drug_era_1_1;30` == 0)

})
