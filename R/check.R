


#' @noRd
checkCdm <- function(cdm, tables = NULL) {
  if (!isTRUE(inherits(cdm, "cdm_reference"))) {
    cli::cli_abort("cdm must be a CDMConnector CDM reference object")
  }
  if (!is.null(tables)) {
    tables <- tables[!(tables %in% names(cdm))]
    if (length(tables) > 0) {
      cli::cli_abort(paste0(
        "tables: ",
        paste0(tables, collapse = ", "),
        "are not present in the cdm object"
      ))
    }
  }
  invisible(NULL)
}


#' @noRd
checkX <- function(x) {
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


}

#' @noRd
checkWindow <- function(window) {
  if (!is.list(window)) {
    window <- list(window)
  }

  # Find if any NA, throw warning that it will be changed to Inf, change it later
  if (any(unlist(lapply(window, is.na)))) {
    cli::cli_abort("NA found in window, please change use Inf or -Inf instead")
  }

  originalWindow <- window
  # change inf to NA to check for floats, as Inf won't pass integerish check
  window <- lapply(window, function(x) replace(x, is.infinite(x), NA))
  checkmate::assertList(window, types = "integerish")
  window <- originalWindow

  # if any element of window list has length over 2, throw error
  if (any(lengths(window) > 2)) {
    cli::cli_abort("window can only contain two values: windowStart and windowEnd")
  }

  # eg if list(1,2,3), change to list(c(1,1), c(2,2), c(3,3))
  if (length(window) > 1 && any(lengths(window) == 1)) {
    window[lengths(window) == 1] <- lapply(
      window[lengths(window) == 1],
      function(x) c(unlist(x[lengths(x) == 1]), unlist(x[lengths(x) == 1]))
    )
    cli::cli_warn("Window list contains element with only 1 value provided,
          use it as both window start and window end")
  }

  windowTbl <- dplyr::tibble(
    lower = lapply(window, function(x) {
      x[1]
    }) %>% unlist(),
    upper = lapply(window, function(x) {
      x[2]
    }) %>% unlist(),
    window_name = getWindowNames(window) %>% unlist()
  ) %>% dplyr::mutate(window_id = dplyr::row_number())


  if (any(windowTbl$lower > windowTbl$upper)) {
    cli::cli_abort("First element in window must be smaller or equal to the second one")
  }
  if (any(is.infinite(windowTbl$lower) & windowTbl$lower == windowTbl$upper & sign(windowTbl$upper) == 1)) {
    cli::cli_abort("Not both elements in the window can be +Inf")
  }
  if (any(is.infinite(windowTbl$lower) & windowTbl$lower == windowTbl$upper & sign(windowTbl$upper) == -1)) {
    cli::cli_abort("Not both elements in the window can be -Inf")
  }

  return(windowTbl)
}


#' @noRd
getWindowNames <- function(window) {
  getname <- function(element) {
    element <- tolower(as.character(element))
    element <- gsub("-", "m", element)
    return(paste0(element[1], "_to_", element[2]))
  }
  windowNames <- names(window)
  if (is.null(windowNames)) {
    windowNames <- lapply(window, getname)
  } else {
    windowNames[windowNames == ""] <- lapply(window[windowNames == ""], getname)
  }
  return(windowNames)
}





#' @noRd
checkCohort <- function(cdm, targetCohortId, targetCohortName) {
  # check targetCohortName
  checkmate::assertCharacter(targetCohortName, len = 1, add = errorMessage)

  # check that targetCohortName point to a table that is a cohort
  checkmate::assertTRUE(
    all(c(
      "cohort_definition_id",
      "subject_id",
      "cohort_start_date",
      "cohort_end_date"
    ) %in% colnames(cdm[[targetCohortName]])),
    add = errorMessage
  )


  # check input cohort cannot have missing in the following columns
  checkmate::assertTRUE(
    !checkmate::anyMissing(cdm[[targetCohortName]] %>% dplyr::pull("cohort_definition_id")),
    add = errorMessage
  )

  checkmate::assertTRUE(
    !checkmate::anyMissing(cdm[[targetCohortName]] %>% dplyr::pull("subject_id")),
    add = errorMessage
  )

  checkmate::assertTRUE(
    !checkmate::anyMissing(cdm[[targetCohortName]] %>% dplyr::pull("cohort_start_date")),
    add = errorMessage
  )

  checkmate::assertTRUE(
    !checkmate::anyMissing(cdm[[targetCohortName]] %>% dplyr::pull("cohort_end_date")),
    add = errorMessage
  )


  # check targetCohortId
  checkmate::assertIntegerish(
    targetCohortId,
    lower = 1,
    null.ok = TRUE,
    add = errorMessage
  )

  # filter the cohort and get the targetCohortId if not specified
  if (!is.null(targetCohortId)) {
    targetCohort <- cdm[[targetCohortName]] %>%
      dplyr::filter(.data$cohort_definition_id %in% .env$targetCohortId)
  } else {
    targetCohort <- cdm[[targetCohortName]]
    targetCohortId <- targetCohort %>%
      dplyr::select("cohort_definition_id") %>%
      dplyr::distinct() %>%
      dplyr::pull()
  }

  parameters <- list(
    "targetCohort" = targetCohort,
    "targetCohortId" = targetCohortId
  )


  return(parameters)
}



checkTable <- function(cdm, tablesToCharacterize) {
  checkmate::assertCharacter(
    tablesToCharacterize,
    min.len = 1
  )
  checkmate::assertTRUE(
    all(tablesToCharacterize %in% names(cdm))
  )
  checkmate::assertTRUE(
    all(tablesToCharacterize %in% namesTable$table_name)
  )
}

checkOverlap <- function(overlap, tablesToCharacterize){

  checkmate::assertLogical(overlap, any.missing = FALSE)

  if (length(overlap) > 1) {
    if (length(overlap) != length(tablesToCharacterize)) {
      stop("If length(overlap)>1 then length(overlap) = length(tablesToCharacterize)")
    }
  } else {
    overlap <- rep(overlap, length(tablesToCharacterize))
  }
  return(overlap)
}
