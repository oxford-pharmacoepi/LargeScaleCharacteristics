library(here)
library(readr)

namesTable <- readr::read_csv(
  here("extras", "namesTable.csv"),
  col_types = list(
    table_name = "c",
    start_date_name = "c",
    end_date_name = "c",
    concept_id_name = "c",
    source_concept_id_name = "c"
  )
)


usethis::use_data(namesTable, internal = TRUE, overwrite = TRUE)
