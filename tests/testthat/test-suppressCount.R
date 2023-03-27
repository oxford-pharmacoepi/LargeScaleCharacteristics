

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

  result <- getLargeScaleCharacteristics(cdm,
    targetCohortName = c("cohort1"),
    tablesToCharacterize = c("drug_era"),
    overlap = FALSE
  )

  result_sup <- suppressCount(result, minimumCellCount = 10)

  expect_true(result_sup$concept_count == "<10")
  expect_true(result_sup$denominator_count == "<10")
})
