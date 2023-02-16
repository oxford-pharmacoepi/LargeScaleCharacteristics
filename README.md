
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
installed then you can install it in the same way:

``` r
remotes::install_github("darwin-eu/CDMConnector")
```

## Example

First, we need to create a cdm_reference for the data we´ll be using.
Here we´ll use generate an example one, but to see how you would set
this up for your database please consult the CDMConnector documentation
at <https://odyosg.github.io/CDMConnector/>

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
# this is what the function output looks like:
#> # A tibble: 3 × 10
#>   cohort_definition_id table_id table_name window_id window_name concept_id concept_name concept_count
#>                  <dbl>    <int> <chr>          <int> <chr>            <dbl>        <int> <chr> 
#> 1                    1        1 drug_expo…         1 Any;-366             1            1 <5  
#> 2                    1        1 drug_expo…         1 Any;-366             3            3 <5 
#> 3                    1        1 drug_expo…         1 Any;-366             5            5 <5  
#> # … with 2 more variables: denominator_count <chr>, concept_type <chr>
```

Add column using function addLargeScaleCharacteristics()
``` r

```

