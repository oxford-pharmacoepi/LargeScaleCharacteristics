
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LargeScaleCharacteristics

<!-- badges: start -->
<!-- badges: end -->

## Package overview

LargeScaleCharacteristics contains functions to get characteristics for cohorts using the OMOP common data
model.

## Package installation

You can install the development version of LargeScaleCharacteristics like so:

``` r
install.packages("remotes")
remotes::install_github("oxford-pharmacoepi/LargeScaleCharacteristics")
```

When working with LargeScaleCharacteristics, you will use CDMConnector to manage
your connection to the database. If you don´t already have this
installed then you can install it and then it can be used to represent mapped database as a single R object:

``` r
remotes::install_github("darwin-eu/CDMConnector")
library(CDMConnector)
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = Sys.getenv("CDM5_POSTGRESQL_DBNAME"),
                      host = Sys.getenv("CDM5_POSTGRESQL_HOST"),
                      user = Sys.getenv("CDM5_POSTGRESQL_USER"),
                      password = Sys.getenv("CDM5_POSTGRESQL_PASSWORD"))
cdm <- CDMConnector::cdm_from_con(con,
                                  cdm_schema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA"))
```

## Example

First, we need to create a cdm_reference for the data we´ll be using. For this example, we´ll generate a hypothetical cohort using mockLargeScaleCharacteristics in this package.
For further details on CDMConnector please refer to <https://odyosg.github.io/CDMConnector/>
``` r
library(CDMConnector)
library(LargeScaleCharacteristics)

# We first need to create a cdm_reference 
cdm <- mockLargeScaleCharacteristics()
# and this is what this example data looks like
head(cdm$person)
#> # Source:   SQL [1 x 5]
#> # Database: DuckDB 0.5.0 [root@Darwin 21.3.0:R 4.2.1/:memory:]
#>   person_id gender_concept_id year_of_birth month_of_birth day_of_birth
#>       <int> <chr>                     <dbl>          <dbl>        <dbl>
#> 1         1 8507                       1994              1           30
head(cdm$cohort1)
#> # Source:   SQL [4 x 4]
#> # Database: DuckDB 0.5.0 [root@Darwin 21.3.0:R 4.2.1/:memory:]
#>   cohort_definition_id subject_id cohort_start_date cohort_end_date
#>                  <dbl>      <dbl> <date>            <date>         
#> 1                    1          1 2020-01-01        2020-04-01     
#> 2                    1          1 2020-06-01        2020-08-01     
#> 3                    1          2 2020-01-02        2020-02-02     
#> 4                    2          3 2020-01-01        2020-03-01 
```

Get large scale characteristic using function getLargeScaleCharacteristics() with output a tiblble with following columns: cohort_definition_id, table_id (in this case 1 as only characteristics from drug_exposure table are requested), window_id, window_name, concept_id, concept_name, concept_count, denominator_count and concept_type
``` r
getLargeScaleCharacteristics(cdm,
  targetCohortName = c("cohort1"),
  overlap = TRUE,
  tablesToCharacterize = "drug_exposure"
)
#> # this is what the function output looks like:
#> # A tibble: 3 × 11
#>   cohort_definition_id table_id table_name    window_id window_name concept_id concept_name   concept_count denominator_count overlap concept_type
#>                  <dbl>    <int> <chr>             <int> <chr>            <dbl> <chr>          <chr>         <chr>             <lgl>   <chr>       
#> 1                    1        1 drug_exposure         1 Any;-366             1 concept_name_1 <5            <5                TRUE    Standard    
#> 2                    1        1 drug_exposure         1 Any;-366             3 concept_name_3 <5            <5                TRUE    Standard    
#> 3                    1        1 drug_exposure         1 Any;-366             5 concept_name_5 <5            <5                TRUE    Standard  
```

Package also allows you to add column to a cohort using table(s) interested in, with function addLargeScaleCharacteristics(). Here we show an example of add covariates from drug exposure table to cohort1. First we show there are 5 different drug_concept_id in drug exposure:
``` r
cdm$drug_exposure %>% dplyr::select(drug_concept_id) %>% dplyr::distinct()
#> # Source:   SQL [5 x 1]
#> # Database: DuckDB 0.5.0 [root@Darwin 21.3.0:R 4.2.1/:memory:]
#>   drug_concept_id
#>             <dbl>
#> 1               1
#> 2               5
#> 3               2
#> 4               4
#> 5               3
```
Hence, the columns we are adding to cohort will have the name form of: {table_name}_{concept_id}_{window_name}. To demonstrate, we only select one window in this example list(c(-30, -1)), and the result looks like this
``` r
addLargeScaleCharacteristics(
  x = cdm$cohort1,
  cdm,
  overlap = TRUE,
  temporalWindows = list(c(-30, -1)),
  tablesToCharacterize = c("drug_exposure")
)
#> # Source:   SQL [4 x 8]
#> # Database: DuckDB 0.5.0 [root@Darwin 21.3.0:R 4.2.1/:memory:]
#>   subject_id cohort_start_date cohort_end_date `drug_exposure_5_-30;-1` `drug_exposure_4_-30;-1` `drug_exposure_3_-30;-1` `drug_exposure_2_-30;-1` `drug_exposure_1_-30;-1`
#>        <dbl> <date>            <date>                             <dbl>                    <dbl>                    <dbl>                    <dbl>                    <dbl>
#> 1          1 2020-06-01        2020-08-01                             0                        0                        0                        0                        0
#> 2          1 2020-01-01        2020-04-01                             0                        0                        0                        0                        0
#> 3          2 2020-01-02        2020-02-02                             0                        0                        0                        0                        0
#> 4          3 2020-01-01        2020-03-01                             0                        0                        0                        0                        0
```

