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

#' This function is used to obscure small counts in population summary tables
#' @param result table with the population summary, must have variables
#' cohort_definition_id, variable, estimate and value
#' @param minimumCellCount number below which the cell results are obscured, defautl: 5
#' @param estimatesToObscure name estimates to obscure if below minimumCellCount, default: concept_count, denominator_count
#'
#' @return table with the required cells obscured
#' @noRd
#'
#' @examples
#' \dontrun{
#' db <- DBI::dbConnect(" Your database connection here")
#' cdm <- CDMConnector::cdm_from_con(
#'   con = db,
#'   cdm_schema = "cdm schema name"
#' )
#' summary_c1 <- getTableOne(cdm, cohort1)
#' summary_obscured <- obscureSummary(summary_c1)
#' }
#'
suppressCount <- function(result,
                         minimumCellCount = 5,
                         cohort_definition_id = NULL,
                         estimatesToObscure = c("concept_count", "denominator_count")) {
  ## check for standard types of user error
  errorMessage <- checkmate::makeAssertCollection()
  column1Check <- c("cohort_definition_id") %in% colnames(result)
  if (!isTRUE(column1Check)) {
    errorMessage$push(
      "- `cohort_definition_id` is not a column of result"
    )
  }

  checkmate::assertIntegerish(minimumCellCount,
    len = 1,
    add = errorMessage,
  )
  checkmate::assertCharacter(estimatesToObscure,
    add = errorMessage,
  )
  checkmate::reportAssertions(collection = errorMessage)

  # Start code
  if (!is.null(cohort_definition_id)) {
    result_to_obscure <- result %>%
      dplyr::filter(cohort_definition_id %in% .env$cohort_definition_id)
  }else{result_to_obscure <- result}


  for (i in estimatesToObscure) {
    result_to_obscure <- result_to_obscure %>%
      dplyr::mutate(obscured_ind = dplyr::if_else(
        .data[[i]] < .env$minimumCellCount, TRUE, FALSE
      )) %>%
      dplyr::mutate({{i}} := as.character(.data[[i]]))%>%
      dplyr::mutate({{i}} := dplyr::if_else(.data$obscured_ind,
                                            paste0("<", minimumCellCount),
                                            .data[[i]]
      )) %>% dplyr::select(-c("obscured_ind"))
    }

  result <- result %>% dplyr::select(-dplyr::all_of({{estimatesToObscure}})) %>%
    dplyr::left_join(result_to_obscure)

  return(result)
}
