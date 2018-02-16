# Overview of dgen

* dgen, or dplyr generator, is intended to be used within the dplyr::mutate command to generate a new variable that is a combination of other variables.

# dgen functions:

## dgen::row_any_count(..., values, maxmiss_n = 0)

  * For each row, returns the count of variables in the variable list (...) equal to a specified value or values. Includes options for handling missing data.

## dgen::row_greater_count(..., value, maxmiss_n = 0)

  * For each row, returns the count of variables in the variable list (...) with values greater than a specified value. Includes options for handling missing data.
  
## dgen::row_less_count(..., value, maxmiss_n = 0)

  * For each row, returns the count of variables in the variable list (...) with values less than a specified value. Includes options for handling missing data.
    
## dgen::row_any_match(..., values, maxmiss_n = 0)

  * Checks whether the value of any variable in a variable list matches a specified value or values. Returns a value of 1 if at least one variable in the variable list (...) is equal to a specified value or values; 0 or NA otherwise. Includes options for handling missing data.

## dgen::row_nmiss(...)

  * Counts the number of missing values within each row.
    
## dgen::row_nvalid(...)

  * Counts the number of non-missing values within each row.

## dgen::row_mean(..., maxmiss_n = 0)

  * Calculates the average value across variables within each row. Includes an option for handling missing data.

## dgen::row_sum(..., maxmiss_n = 0)

  * Calculates sum across variables within each row. Includes an option for handling missing data.
